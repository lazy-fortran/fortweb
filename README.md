# fortweb

HTTP server building blocks in Fortran over POSIX sockets, with no C sources
and no upstream dependencies beyond the C library. [Source](https://github.com/lazy-fortran/fortweb).
It serves [FortShell](https://github.com/lazy-fortran/fortshell). Linux only.

| Module | Contents |
| --- | --- |
| `fortweb_posix` | TCP listen, accept, connect, send and receive, socket activation |
| `fortweb_http` | HTTP/1.1 request parsing and responses, one request per connection |
| `fortweb_server` | Pre-forked processes around one handler procedure, replaced when they die |
| `fortweb_static` | Files below a root with path containment, content types and SHA-256 validators |
| `fortweb_sse` | Server-sent event streams that follow a directory through inotify |
| `fortweb_proc` | fork, wait and kernel random tokens |

```fortran
use fortweb_posix, only: tcp_listen
use fortweb_server, only: web_serve
use fortweb_static, only: web_send_file

call tcp_listen('127.0.0.1', 8080, lfd, stat)
call web_serve(lfd, handler, processes=8)

contains

subroutine handler(fd, req, rest)
    integer, intent(in) :: fd
    type(http_request_t), intent(inout) :: req
    character(len=*), intent(in) :: rest
    call web_send_file(fd, req, '/srv/site')
end subroutine handler
```

Each process serves one connection at a time, so a long event stream holds
one process. TLS is terminated by a reverse proxy until fortweb's own TLS 1.3
passes its conformance and interoperability tests.

Build and test with [fo](https://github.com/lazy-fortran/fo): `fo test`. The
request parsers are compiled with bounds checks in every profile.
