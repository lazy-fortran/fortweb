program test_serve
    !! A forked server answers files, revalidation and an event stream over a
    !! real socket.
    use, intrinsic :: iso_c_binding, only: c_int
    use fortweb_http, only: http_request_t, http_send_response
    use fortweb_posix, only: sock_bound_port, sock_close, sock_recv, sock_send_all, &
        sock_set_timeouts, tcp_connect, tcp_listen
    use fortweb_proc, only: proc_exit, proc_fork, proc_pid, proc_sleep_ms
    use fortweb_server, only: web_serve
    use fortweb_sse, only: sse_begin, sse_follow_dir, sse_send
    use fortweb_static, only: web_send_file
    implicit none

    interface
        function c_setpgid(pid, pgid) bind(C, name='setpgid') result(rc)
            import :: c_int
            integer(c_int), value :: pid, pgid
            integer(c_int) :: rc
        end function c_setpgid

        function c_kill(pid, sig) bind(C, name='kill') result(rc)
            import :: c_int
            integer(c_int), value :: pid, sig
            integer(c_int) :: rc
        end function c_kill
    end interface

    character(len=2), parameter :: CRLF = achar(13)//achar(10)
    character(len=:), allocatable :: root, reply, etag
    integer :: lfd, port, stat, pid, unit, fd, k, count
    character(len=64) :: name
    integer(c_int) :: rc

    write (name, '(a,i0)') 'test_serve_root_', proc_pid()
    root = trim(name)
    call execute_command_line('mkdir -p '//root)
    open (newunit=unit, file=root//'/index.html', status='replace', action='write')
    write (unit, '(a)') '<p>ink</p>'
    close (unit)

    call tcp_listen('127.0.0.1', 0, lfd, stat)
    call check(stat == 0, 'listen on an ephemeral port')
    port = sock_bound_port(lfd)

    pid = proc_fork()
    if (pid == 0) then
        rc = c_setpgid(0_c_int, 0_c_int)
        call web_serve(lfd, handler, 2, timeout=5)
        call proc_exit(0)
    end if
    call sock_close(lfd)

    reply = request('GET / HTTP/1.1'//CRLF//'Host: t'//CRLF//CRLF)
    call check(index(reply, 'HTTP/1.1 200') == 1, 'file answered')
    call check(index(reply, 'text/html') > 0, 'html content type')
    call check(index(reply, '<p>ink</p>') > 0, 'file body')
    k = index(reply, 'ETag: ')
    call check(k > 0, 'validator sent')
    etag = reply(k + 6:k + 5 + index(reply(k + 6:), CRLF) - 1)

    reply = request('GET /index.html HTTP/1.1'//CRLF//'If-None-Match: '//etag//CRLF//CRLF)
    call check(index(reply, 'HTTP/1.1 304') == 1, 'unchanged file revalidates')
    call check(index(reply, '<p>ink</p>') == 0, 'no body on 304')

    reply = request('HEAD / HTTP/1.1'//CRLF//CRLF)
    call check(index(reply, 'HTTP/1.1 200') == 1 .and. index(reply, '<p>') == 0, 'HEAD has no body')

    reply = request('GET /%2e%2e/x HTTP/1.1'//CRLF//CRLF)
    call check(index(reply, 'HTTP/1.1 404') == 1, 'escape attempt not found')

    reply = request('garbage'//CRLF//CRLF)
    call check(index(reply, 'HTTP/1.1 400') == 1, 'malformed head answered by the server')

    ! The stream sends one event at once and one after a file lands.
    call tcp_connect('127.0.0.1', port, fd, stat)
    call check(stat == 0, 'connect for events')
    call sock_set_timeouts(fd, 5)
    call sock_send_all(fd, 'GET /events HTTP/1.1'//CRLF//CRLF, stat)
    reply = read_until(fd, 'data: 1')
    call check(index(reply, 'text/event-stream') > 0, 'event stream head')
    call check(index(reply, 'data: 1') > 0, 'first event')
    open (newunit=unit, file=root//'/new.txt', status='replace', action='write')
    write (unit, '(a)') 'x'
    close (unit)
    reply = read_until(fd, 'data: 2')
    call check(index(reply, 'data: 2') > 0, 'event after a change')
    call sock_close(fd)

    rc = c_kill(-int(pid, c_int), 15_c_int)
    call execute_command_line('rm -rf '//root)

contains

    subroutine handler(fd, req, rest)
        integer, intent(in) :: fd
        type(http_request_t), intent(inout) :: req
        character(len=*), intent(in) :: rest
        integer :: stat

        if (req%path == '/events') then
            count = 0
            call sse_begin(fd, stat)
            if (stat == 0) call sse_follow_dir(fd, root, changed, 1000)
            return
        end if
        call web_send_file(fd, req, root)
    end subroutine handler

    subroutine changed(fd, stat)
        integer, intent(in) :: fd
        integer, intent(out) :: stat
        character(len=12) :: n

        count = count + 1
        write (n, '(i0)') count
        call sse_send(fd, 'change', trim(n), stat)
    end subroutine changed

    function request(text) result(reply)
        character(len=*), intent(in) :: text
        character(len=:), allocatable :: reply
        character(len=4096) :: buf
        integer :: fd, stat, n

        reply = ''
        call tcp_connect('127.0.0.1', port, fd, stat)
        call check(stat == 0, 'connect')
        call sock_set_timeouts(fd, 5)
        call sock_send_all(fd, text, stat)
        do
            n = sock_recv(fd, buf)
            if (n <= 0) exit
            reply = reply//buf(1:n)
        end do
        call sock_close(fd)
    end function request

    function read_until(fd, marker) result(got)
        integer, intent(in) :: fd
        character(len=*), intent(in) :: marker
        character(len=:), allocatable :: got
        character(len=4096) :: buf
        integer :: n

        got = ''
        do while (index(got, marker) == 0)
            n = sock_recv(fd, buf)
            if (n <= 0) exit
            got = got//buf(1:n)
        end do
    end function read_until

    subroutine check(ok, what)
        logical, intent(in) :: ok
        character(len=*), intent(in) :: what

        if (.not. ok) then
            write (*, '(a)') 'FAIL: '//what
            rc = c_kill(-int(pid, c_int), 15_c_int)
            error stop 1
        end if
    end subroutine check

end program test_serve
