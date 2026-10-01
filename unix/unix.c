// Unix domain sockets
// ===================

#include <sys/stat.h>
#include <sys/un.h>

// path as a sockaddr_un; -1 when it does not fit in sun_path (104 bytes on
// macOS, 108 on Linux, counting the final NUL).
static int unix_addr(const char* path, u64 len, struct sockaddr_un* at) {
  memset(at, 0, sizeof(*at));
  at->sun_family = AF_UNIX;
  if (len == 0 || len >= sizeof(at->sun_path)) {
    return -1;
  }
  memcpy(at->sun_path, path, len);
  return 0;
}

static Term unix_fail(Env e, char* path, uint32_t code) {
  free(path);
  return io_fail(e, code, NULL);
}

Term unix_listen_run(Env e, Term* f, IoWork* w) {
  struct sockaddr_un at;
  struct stat        st;
  u64   len;
  char* path = io_cstr(e, f[0], &len);
  if (io_nul(path, len)) {
    return unix_fail(e, path, EINVAL);
  }
  if (unix_addr(path, len, &at) < 0) {
    return unix_fail(e, path, ENAMETOOLONG);
  }
  // A socket file is taken over only when nothing answers on it any more.
  if (lstat(path, &st) == 0) {
    if (!S_ISSOCK(st.st_mode)) {
      return unix_fail(e, path, EEXIST);
    }
    int probe = socket(AF_UNIX, SOCK_STREAM, 0);
    int live  = probe >= 0 && connect(probe, (struct sockaddr*)&at, sizeof(at)) == 0;
    if (probe >= 0) {
      close(probe);
    }
    if (live) {
      return unix_fail(e, path, EADDRINUSE);
    }
    unlink(path);
  }
  int fd = socket(AF_UNIX, SOCK_STREAM, 0);
  if (fd < 0) {
    return unix_fail(e, path, (uint32_t)errno);
  }
  // Born 0600, so no other user can connect between bind and chmod.
  mode_t old   = umask(0177);
  int    bound = bind(fd, (struct sockaddr*)&at, sizeof(at));
  umask(old);
  if (bound < 0 || chmod(path, 0600) < 0 || listen(fd, 512) < 0
    || fcntl(fd, F_SETFL, fcntl(fd, F_GETFL) | O_NONBLOCK) < 0) {
    uint32_t code = (uint32_t)errno;
    close(fd);
    if (bound == 0) {
      unlink(path);
    }
    return unix_fail(e, path, code);
  }
  free(path);
  return io_done(e, io_hand(fd));
}

Term unix_connect_run(Env e, Term* f, IoWork* w) {
  struct sockaddr_un at;
  u64   len;
  char* path = io_cstr(e, f[0], &len);
  if (io_nul(path, len)) {
    return unix_fail(e, path, EINVAL);
  }
  if (unix_addr(path, len, &at) < 0) {
    return unix_fail(e, path, ENAMETOOLONG);
  }
  int fd = socket(AF_UNIX, SOCK_STREAM, 0);
  if (fd < 0 || connect(fd, (struct sockaddr*)&at, sizeof(at)) < 0
    || fcntl(fd, F_SETFL, fcntl(fd, F_GETFL) | O_NONBLOCK) < 0) {
    uint32_t code = (uint32_t)errno;
    if (fd >= 0) {
      close(fd);
    }
    return unix_fail(e, path, code);
  }
  free(path);
  return io_done(e, io_hand(fd));
}

// A def the program never calls has no id, and is not registered.
static void __attribute__((constructor)) unix_use(void) {
#ifdef CID(listen)
  io_eff(CID(listen), unix_listen_run, 0);
#endif
#ifdef CID(connect)
  io_eff(CID(connect), unix_connect_run, 0);
#endif
}
