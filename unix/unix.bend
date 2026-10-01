# Unix domain sockets, as Base's Listener and Socket: TCP.accept, Wire.recv,
# Wire.send and Socket.close work on them unchanged.
#   import ./unix/unix.bend as Unix
import Base

# Listens on path, a socket file only its owner may use (mode 0600). A socket file
# that nothing answers on any more is replaced; a live one fails with EADDRINUSE and
# any other file with EEXIST. A path too long for sun_path fails with ENAMETOOLONG.
def listen(path: String) -> IO(Result<&1, &1, U32 & String, Listener>):
  import "./unix.c"
  import "./unix.js"

# Connects to the socket at path.
def connect(path: String) -> IO(Result<&1, &1, U32 & String, Socket>):
  import "./unix.c"
  import "./unix.js"
