module fortweb_proc
    !! Processes and kernel randomness through the C library (Linux).
    use, intrinsic :: iso_c_binding
    implicit none
    private

    public :: &
        proc_exit, &
        proc_fork, &
        proc_pid, &
        proc_sleep_ms, &
        proc_wait_any, &
        random_hex

    interface
        function c_fork() bind(C, name='fork') result(pid)
            import :: c_int
            integer(c_int) :: pid
        end function c_fork

        function c_getpid() bind(C, name='getpid') result(pid)
            import :: c_int
            integer(c_int) :: pid
        end function c_getpid

        function c_waitpid(pid, status, options) bind(C, name='waitpid') result(rc)
            import :: c_int
            integer(c_int), value :: pid, options
            integer(c_int), intent(out) :: status
            integer(c_int) :: rc
        end function c_waitpid

        subroutine c_exit(status) bind(C, name='_exit')
            import :: c_int
            integer(c_int), value :: status
        end subroutine c_exit

        function c_usleep(usec) bind(C, name='usleep') result(rc)
            import :: c_int
            integer(c_int), value :: usec
            integer(c_int) :: rc
        end function c_usleep

        function c_getrandom(buf, n, flags) bind(C, name='getrandom') result(got)
            import :: c_char, c_size_t, c_int, c_intptr_t
            character(kind=c_char), intent(out) :: buf(*)
            integer(c_size_t), value :: n
            integer(c_int), value :: flags
            integer(c_intptr_t) :: got
        end function c_getrandom
    end interface

contains

    function proc_fork() result(pid)
        !! 0 in the child, the child's id in the parent, negative on failure.
        integer :: pid

        pid = int(c_fork())
    end function proc_fork

    function proc_pid() result(pid)
        integer :: pid

        pid = int(c_getpid())
    end function proc_pid

    subroutine proc_wait_any(pid, status)
        !! Blocks until a child ends; pid is negative when there is none.
        integer, intent(out) :: pid, status
        integer(c_int) :: st

        st = 0
        pid = int(c_waitpid(-1_c_int, st, 0_c_int))
        status = int(st)
    end subroutine proc_wait_any

    subroutine proc_exit(status)
        !! Ends the process at once, without flushing the parent's buffers twice.
        integer, intent(in) :: status

        call c_exit(int(status, c_int))
    end subroutine proc_exit

    subroutine proc_sleep_ms(ms)
        integer, intent(in) :: ms
        integer(c_int) :: rc

        rc = c_usleep(int(1000 * ms, c_int))
    end subroutine proc_sleep_ms

    function random_hex(nbytes, stat) result(hex)
        !! `nbytes` bytes from the kernel's random source as lowercase hex.
        integer, intent(in) :: nbytes
        integer, intent(out) :: stat
        character(len=:), allocatable :: hex
        character(kind=c_char) :: buf(nbytes)
        character(len=16), parameter :: DIGITS = '0123456789abcdef'
        integer(c_intptr_t) :: got
        integer :: i, b, filled

        hex = ''
        stat = 1
        filled = 0
        do while (filled < nbytes)
            got = c_getrandom(buf(filled + 1:), int(nbytes - filled, c_size_t), 0_c_int)
            if (got <= 0) return
            filled = filled + int(got)
        end do
        hex = repeat(" ", 2*nbytes)
        do i = 1, nbytes
            b = ichar(buf(i))
            hex(2*i - 1:2*i - 1) = DIGITS(b/16 + 1:b/16 + 1)
            hex(2*i:2*i) = DIGITS(mod(b, 16) + 1:mod(b, 16) + 1)
        end do
        stat = 0
    end function random_hex

end module fortweb_proc
