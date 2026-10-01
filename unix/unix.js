// Unix domain sockets
// ===================

// path as a sockaddr_un: on macOS a length byte, the family and 104 bytes of
// path; on Linux a 16-bit family and 108. null when it does not fit.
function unix_addr(path) {
  const sys = io_sys();
  const b   = io_bytes(path);
  const cap = sys.mac ? 104 : 108;
  if (b.length === 0 || b.length >= cap) {
    return null;
  }
  const at = new Uint8Array(cap + 2);
  at.set(sys.mac ? [cap + 2, 1] : [1, 0]);
  at.set(b, 2);
  return at;
}

function unix_code(name) {
  const mac = io_sys().mac;
  return { EINVAL: 22, EEXIST: 17, EADDRINUSE: mac ? 48 : 98,
    ENAMETOOLONG: mac ? 63 : 36 }[name];
}

function unix_listen(path) {
  const fs  = require("node:fs");
  const sys = io_sys();
  if (path.includes("\0")) {
    return io_fail(unix_code("EINVAL"));
  }
  const at = unix_addr(path);
  if (at === null) {
    return io_fail(unix_code("ENAMETOOLONG"));
  }
  // A socket file is taken over only when nothing answers on it any more.
  let st = null;
  try { st = fs.lstatSync(path); } catch {}
  if (st !== null) {
    if (!st.isSocket()) {
      return io_fail(unix_code("EEXIST"));
    }
    const probe = sys.socket(1, 1, 0);
    const live  = probe >= 0 && sys.connect(probe, sys.ptr(at), at.length) === 0;
    if (probe >= 0) {
      sys.close(probe);
    }
    if (live) {
      return io_fail(unix_code("EADDRINUSE"));
    }
    fs.unlinkSync(path);
  }
  const fd = sys.socket(1, 1, 0);
  if (fd < 0) {
    return io_fail(sys.errno());
  }
  // Born 0600, so no other user can connect between bind and chmod.
  const old   = process.umask(0o177);
  const bound = sys.bind(fd, sys.ptr(at), at.length);
  process.umask(old);
  let code = 0;
  if (bound < 0) {
    code = sys.errno();
  } else {
    try { fs.chmodSync(path, 0o600); } catch (e) { code = -e.errno || 1; }
  }
  if (code === 0 && (sys.listen(fd, 512) < 0
    || sys.fcntl(fd, 4, sys.fcntl(fd, 3, 0) | (sys.mac ? 4 : 0x800)) < 0)) {
    code = sys.errno();
  }
  if (code !== 0) {
    sys.close(fd);
    if (bound === 0) {
      try { fs.unlinkSync(path); } catch {}
    }
    return io_fail(code);
  }
  return io_done(fd);
}

function unix_connect(path) {
  const sys = io_sys();
  if (path.includes("\0")) {
    return io_fail(unix_code("EINVAL"));
  }
  const at = unix_addr(path);
  if (at === null) {
    return io_fail(unix_code("ENAMETOOLONG"));
  }
  const fd = sys.socket(1, 1, 0);
  if (fd < 0 || sys.connect(fd, sys.ptr(at), at.length) < 0
    || sys.fcntl(fd, 4, sys.fcntl(fd, 3, 0) | (sys.mac ? 4 : 0x800)) < 0) {
    const code = sys.errno();
    if (fd >= 0) {
      sys.close(fd);
    }
    return io_fail(code);
  }
  return io_done(fd);
}

io_eff(CID(listen), unix_listen);
io_eff(CID(connect), unix_connect);
