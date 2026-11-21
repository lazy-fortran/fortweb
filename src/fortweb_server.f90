module fortweb_server
    !! Pre-forked HTTP serving around one handler procedure. Each process
    !! serves one connection at a time, so a long-lived event stream holds one
    !! process, and a process that dies, for example on a Fortran runtime
    !! error, loses only its own request and is replaced. Not threads: gfortran
    !! keeps the length of deferred-length character results in static
    !! storage, so concurrent calls corrupt each other's strings.
    use, intrinsic :: iso_fortran_env, only: int64, error_unit, output_unit
    use fortweb_http, only: http_read_head, http_request_t, http_send_response
    use fortweb_posix, only: sock_close, sock_set_timeouts, tcp_accept
    use fortweb_proc, only: proc_exit, proc_fork, proc_sleep_ms, proc_wait_any
    implicit none
    private

    public :: web_handler, web_serve, web_serve_connection

    abstract interface
        subroutine web_handler(fd, req, rest)
            !! Answers one request on socket `fd`. `rest` holds the body bytes
            !! read with the head; http_read_body reads the remainder.
            import :: http_request_t
            integer, intent(in) :: fd
            type(http_request_t), intent(inout) :: req
            character(len=*), intent(in) :: rest
        end subroutine web_handler
    end interface

contains

    subroutine web_serve(lfd, handler, processes, timeout)
        !! Serves the listening socket `lfd` from `processes` forked processes
        !! until the supervisor is killed. Under systemd, stopping the unit
        !! ends the whole process group. `timeout` bounds each socket read and
        !! write in seconds; event streams send keep-alives within it.
        integer, intent(in) :: lfd, processes
        procedure(web_handler) :: handler
        integer, intent(in), optional :: timeout
        integer :: pids(processes), i, pid, status, seconds
        integer(int64) :: started(processes), now, rate

        seconds = 30
        if (present(timeout)) seconds = timeout
        do i = 1, processes
            call spawn(i)
        end do
        do
            call proc_wait_any(pid, status)
            if (pid <= 0) then
                call proc_sleep_ms(200)
                cycle
            end if
            do i = 1, processes
                if (pids(i) /= pid) cycle
                write (error_unit, '(a,i0,a,i0)') 'fortweb: process ', pid, &
                    ' ended with wait status ', status
                call system_clock(now, rate)
                ! A process that dies within a second of starting is crash-looping.
                if (now - started(i) < rate) call proc_sleep_ms(1000)
                call spawn(i)
            end do
        end do

    contains

        subroutine spawn(slot)
            integer, intent(in) :: slot
            integer :: cfd

            do
                flush (error_unit)
                flush (output_unit)
                call system_clock(started(slot), rate)
                pids(slot) = proc_fork()
                if (pids(slot) >= 0) exit
                write (error_unit, '(a)') 'fortweb: cannot fork; retrying'
                call proc_sleep_ms(1000)
            end do
            if (pids(slot) /= 0) return
            do
                cfd = tcp_accept(lfd)
                if (cfd < 0) cycle
                call sock_set_timeouts(cfd, seconds)
                call web_serve_connection(cfd, handler)
                call sock_close(cfd)
            end do
            call proc_exit(0)
        end subroutine spawn

    end subroutine web_serve

    subroutine web_serve_connection(cfd, handler)
        !! Reads one request head from `cfd` and hands it to `handler`, or
        !! answers a malformed head itself.
        integer, intent(in) :: cfd
        procedure(web_handler) :: handler
        type(http_request_t) :: req
        character(len=:), allocatable :: rest
        integer :: status

        call http_read_head(cfd, req, rest, status)
        if (status < 0) return
        if (status /= 0) then
            call http_send_response(cfd, status, 'text/plain; charset=utf-8', 'bad request')
            return
        end if
        call handler(cfd, req, rest)
    end subroutine web_serve_connection

end module fortweb_server
