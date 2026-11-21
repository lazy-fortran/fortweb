program test_static_paths
    use fortweb_static, only: web_content_type, web_file_path, web_read_file
    use fortweb_proc, only: random_hex
    implicit none

    character(len=:), allocatable :: path, data, a, b
    logical :: ok
    integer :: stat, unit, i

    path = web_file_path('/srv/w', '/', ok)
    call check(ok .and. path == '/srv/w/index.html', 'root maps to index.html')
    path = web_file_path('/srv/w', '/present/deck/p01.svg', ok)
    call check(ok .and. path == '/srv/w/present/deck/p01.svg', 'nested file')
    path = web_file_path('/srv/w', '/a/', ok)
    call check(ok .and. path == '/srv/w/a/index.html', 'directory maps to index.html')
    path = web_file_path('/srv/w', '/a/../../etc/passwd', ok)
    call check(.not. ok, 'dot-dot segment refused')
    path = web_file_path('/srv/w', '/a/%2e%2e/b', ok)
    call check(.not. ok, 'encoded dot-dot refused')
    path = web_file_path('/srv/w', '/.git/config', ok)
    call check(.not. ok, 'hidden name refused')
    path = web_file_path('/srv/w', '/a%00b', ok)
    call check(.not. ok, 'NUL refused')
    path = web_file_path('/srv/w', '/a\b', ok)
    call check(.not. ok, 'backslash refused')
    path = web_file_path('/srv/w', '//etc/passwd', ok)
    call check(.not. ok, 'empty segment refused')
    path = web_file_path('/srv/w', 'index.html', ok)
    call check(.not. ok, 'relative path refused')

    call check(web_content_type('a/b.wasm') == 'application/wasm', 'wasm type')
    call check(web_content_type('a/b.mjs') == 'text/javascript; charset=utf-8', 'module type')
    call check(web_content_type('a/b.svg') == 'image/svg+xml', 'svg type')
    call check(web_content_type('a.b/c') == 'application/octet-stream', 'dot in directory only')

    ! Round trip through a generated file with every byte value.
    data = ''
    do i = 0, 1023
        data = data//achar(mod(i, 256))
    end do
    open (newunit=unit, file='test_static_paths.bin', access='stream', form='unformatted', &
        status='replace', action='write')
    write (unit) data
    close (unit)
    call web_read_file('test_static_paths.bin', path, stat)
    call check(stat == 0 .and. path == data, 'binary file read back unchanged')
    open (newunit=unit, file='test_static_paths.bin', status='old')
    close (unit, status='delete')
    call web_read_file('test_static_paths.missing', path, stat)
    call check(stat /= 0, 'missing file reported')

    a = random_hex(32, stat)
    call check(stat == 0 .and. len(a) == 64, 'random token length')
    call check(verify(a, '0123456789abcdef') == 0, 'random token is hex')
    b = random_hex(32, stat)
    call check(a /= b, 'two random tokens differ')

contains

    subroutine check(ok, what)
        logical, intent(in) :: ok
        character(len=*), intent(in) :: what

        if (.not. ok) then
            write (*, '(a)') 'FAIL: '//what
            error stop 1
        end if
    end subroutine check

end program test_static_paths
