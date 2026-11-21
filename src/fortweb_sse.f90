module fortweb_sse
    !! Server-sent event streams that follow changes in a directory (Linux
    !! inotify). The stream ends when the client goes away.
    use, intrinsic :: iso_c_binding
    use fortweb_posix, only: sock_send_all
    implicit none
    private

    public :: sse_begin, sse_follow_dir, sse_on_change, sse_send

    character(len=2), parameter :: CRLF = achar(13)//achar(10)
    character(len=1), parameter :: LF = achar(10)
    integer(c_int), parameter :: IN_CLOEXEC = int(o'2000000', c_int)
    integer(c_int), parameter :: IN_CLOSE_WRITE = 8, IN_MOVED_TO = 128, IN_MODIFY = 2
    integer(c_short), parameter :: POLLIN = 1

    type, bind(C) :: pollfd_t
        integer(c_int) :: fd
        integer(c_short) :: events, revents
    end type pollfd_t

    abstract interface
        subroutine sse_on_change(fd, stat)
            !! Sends whatever changed on stream `fd`; nonzero stat ends it.
            integer, intent(in) :: fd
            integer, intent(out) :: stat
        end subroutine sse_on_change
    end interface

    interface
        function c_inotify_init1(flags) bind(C, name='inotify_init1') result(fd)
            import :: c_int
            integer(c_int), value :: flags
            integer(c_int) :: fd
        end function c_inotify_init1

        function c_inotify_add_watch(fd, path, mask) bind(C, name='inotify_add_watch') result(wd)
            import :: c_int, c_char, c_int32_t
            integer(c_int), value :: fd
            character(kind=c_char), intent(in) :: path(*)
            integer(c_int32_t), value :: mask
            integer(c_int) :: wd
        end function c_inotify_add_watch

        function c_poll(fds, n, timeout) bind(C, name='poll') result(rc)
            import :: pollfd_t, c_long, c_int
            type(pollfd_t), intent(inout) :: fds(*)
            integer(c_long), value :: n
            integer(c_int), value :: timeout
            integer(c_int) :: rc
        end function c_poll

        function c_read(fd, buf, n) bind(C, name='read') result(got)
            import :: c_int, c_char, c_size_t, c_intptr_t
            integer(c_int), value :: fd
            character(kind=c_char), intent(out) :: buf(*)
            integer(c_size_t), value :: n
            integer(c_intptr_t) :: got
        end function c_read

        function c_close(fd) bind(C, name='close') result(rc)
            import :: c_int
            integer(c_int), value :: fd
            integer(c_int) :: rc
        end function c_close
    end interface

contains

    subroutine sse_begin(fd, stat)
        !! The response head of an event stream, with a reconnect delay.
        integer, intent(in) :: fd
        integer, intent(out) :: stat

        call sock_send_all(fd, 'HTTP/1.1 200 OK'//CRLF//'Content-Type: text/event-stream'//CRLF// &
            'Cache-Control: no-store'//CRLF//'X-Accel-Buffering: no'//CRLF// &
            'Connection: close'//CRLF//CRLF//'retry: 2000'//LF//LF, stat)
    end subroutine sse_begin

    subroutine sse_send(fd, event, data, stat, id)
        !! One event. `data` must not contain line breaks; JSON on one line does.
        integer, intent(in) :: fd
        character(len=*), intent(in) :: event, data
        integer, intent(out) :: stat
        character(len=*), intent(in), optional :: id
        character(len=:), allocatable :: msg

        msg = ''
        if (present(id)) msg = 'id: '//id//LF
        if (len(event) > 0) msg = msg//'event: '//event//LF
        call sock_send_all(fd, msg//'data: '//data//LF//LF, stat)
    end subroutine sse_send

    subroutine sse_follow_dir(fd, dir, on_change, keepalive_ms)
        !! Calls `on_change` once at once and again whenever a file in `dir` is
        !! written or moved in, until the client closes or a send fails. Sends
        !! a comment every `keepalive_ms` so proxies and timeouts keep the
        !! stream open.
        integer, intent(in) :: fd
        character(len=*), intent(in) :: dir
        procedure(sse_on_change) :: on_change
        integer, intent(in) :: keepalive_ms
        type(pollfd_t) :: fds(2)
        character(kind=c_char) :: buf(4096)
        integer(c_int) :: ino, wd, rc
        integer(c_intptr_t) :: got
        integer :: stat

        call on_change(fd, stat)
        if (stat /= 0) return
        ino = c_inotify_init1(IN_CLOEXEC)
        if (ino < 0) return
        wd = c_inotify_add_watch(ino, dir//c_null_char, &
            int(ior(ior(IN_CLOSE_WRITE, IN_MOVED_TO), IN_MODIFY), c_int32_t))
        if (wd >= 0) then
            do
                fds(1) = pollfd_t(ino, POLLIN, 0_c_short)
                fds(2) = pollfd_t(int(fd, c_int), POLLIN, 0_c_short)
                rc = c_poll(fds, 2_c_long, int(keepalive_ms, c_int))
                if (rc < 0) exit
                if (rc == 0) then
                    call sock_send_all(fd, ': keep-alive'//LF//LF, stat)
                    if (stat /= 0) exit
                    cycle
                end if
                ! An event stream's client never sends: readable means it closed.
                if (fds(2)%revents /= 0) exit
                if (fds(1)%revents /= 0) then
                    got = c_read(ino, buf, int(size(buf), c_size_t))
                    call on_change(fd, stat)
                    if (stat /= 0) exit
                end if
            end do
        end if
        rc = c_close(ino)
    end subroutine sse_follow_dir

end module fortweb_sse
