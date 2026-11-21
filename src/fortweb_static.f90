module fortweb_static
    !! Files under a root directory, with content types and validators.
    use fortweb_http, only: http_percent_decode, http_request_t, http_send_response
    use fortweb_posix, only: sock_send_all
    use fx_hash, only: sha256_string
    implicit none
    private

    public :: web_content_type, web_file_path, web_read_file, web_send_file

    character(len=2), parameter :: CRLF = achar(13)//achar(10)

contains

    function web_file_path(root, url_path, ok) result(path)
        !! The file below `root` that `url_path` names, with index.html for a
        !! directory. Refuses dot segments, hidden names, backslashes and
        !! control bytes, so no request reaches outside `root`.
        character(len=*), intent(in) :: root, url_path
        logical, intent(out) :: ok
        character(len=:), allocatable :: path, p
        integer :: i

        path = ''
        ok = .false.
        p = http_percent_decode(url_path)
        if (len(p) == 0) return
        if (p(1:1) /= '/') return
        do i = 1, len(p)
            if (iachar(p(i:i)) < 32 .or. iachar(p(i:i)) == 127) return
            if (p(i:i) == '\') return
            if (p(i:i) == '.' .and. i > 1) then
                if (p(i - 1:i - 1) == '/') return
            end if
            if (p(i:i) == '/' .and. i > 1) then
                if (p(i - 1:i - 1) == '/') return
            end if
        end do
        if (p(len(p):len(p)) == '/') p = p//'index.html'
        path = root//p
        ok = .true.
    end function web_file_path

    pure function web_content_type(path) result(ctype)
        character(len=*), intent(in) :: path
        character(len=:), allocatable :: ctype
        integer :: dot

        dot = index(path, '.', back=.true.)
        ctype = 'application/octet-stream'
        if (dot == 0 .or. dot < index(path, '/', back=.true.)) return
        select case (path(dot + 1:))
        case ('html')
            ctype = 'text/html; charset=utf-8'
        case ('js', 'mjs')
            ctype = 'text/javascript; charset=utf-8'
        case ('css')
            ctype = 'text/css; charset=utf-8'
        case ('json', 'map')
            ctype = 'application/json'
        case ('webmanifest')
            ctype = 'application/manifest+json'
        case ('wasm')
            ctype = 'application/wasm'
        case ('svg')
            ctype = 'image/svg+xml'
        case ('png')
            ctype = 'image/png'
        case ('jpg', 'jpeg')
            ctype = 'image/jpeg'
        case ('webp')
            ctype = 'image/webp'
        case ('pdf')
            ctype = 'application/pdf'
        case ('mp4')
            ctype = 'video/mp4'
        case ('woff2')
            ctype = 'font/woff2'
        case ('txt')
            ctype = 'text/plain; charset=utf-8'
        end select
    end function web_content_type

    subroutine web_read_file(path, data, stat)
        !! The whole regular file `path`; stat is nonzero when it cannot be read.
        character(len=*), intent(in) :: path
        character(len=:), allocatable, intent(out) :: data
        integer, intent(out) :: stat
        integer :: unit, size_

        data = ''
        open (newunit=unit, file=path, access='stream', form='unformatted', &
            status='old', action='read', iostat=stat)
        if (stat /= 0) return
        inquire (unit=unit, size=size_)
        if (size_ < 0) then
            stat = 1
            close (unit)
            return
        end if
        data = repeat(" ", size_)
        if (size_ > 0) read (unit, iostat=stat) data
        close (unit)
    end subroutine web_read_file

    subroutine web_send_file(fd, req, root, cache)
        !! Answers GET or HEAD for the file below `root` that the request
        !! names. The validator is the content's SHA-256, so an unchanged file
        !! costs the client one round trip and no transfer. `cache` is the
        !! Cache-Control value, no-cache unless given.
        integer, intent(in) :: fd
        type(http_request_t), intent(in) :: req
        character(len=*), intent(in) :: root
        character(len=*), intent(in), optional :: cache
        character(len=:), allocatable :: path, data, etag, policy
        character(len=20) :: clen
        logical :: ok
        integer :: stat

        policy = 'no-cache'
        if (present(cache)) policy = cache
        if (req%method /= 'GET' .and. req%method /= 'HEAD') then
            call http_send_response(fd, 405, 'text/plain; charset=utf-8', 'method not allowed', &
                'Allow: GET, HEAD'//CRLF)
            return
        end if
        path = web_file_path(root, req%path, ok)
        if (ok) call web_read_file(path, data, stat)
        if (.not. ok .or. stat /= 0) then
            call http_send_response(fd, 404, 'text/plain; charset=utf-8', 'not found')
            return
        end if
        etag = '"'//sha256_string(data)//'"'
        if (allocated(req%if_none_match)) then
            if (req%if_none_match == etag) then
                call sock_send_all(fd, 'HTTP/1.1 304 Not Modified'//CRLF//'ETag: '//etag//CRLF// &
                    'Cache-Control: '//policy//CRLF//'Connection: close'//CRLF//CRLF, stat)
                return
            end if
        end if
        write (clen, '(i0)') len(data)
        call sock_send_all(fd, 'HTTP/1.1 200 OK'//CRLF// &
            'Content-Type: '//web_content_type(path)//CRLF// &
            'Content-Length: '//trim(clen)//CRLF// &
            'ETag: '//etag//CRLF// &
            'Cache-Control: '//policy//CRLF// &
            'X-Content-Type-Options: nosniff'//CRLF// &
            'Connection: close'//CRLF//CRLF, stat)
        if (stat == 0 .and. req%method == 'GET') call sock_send_all(fd, data, stat)
    end subroutine web_send_file

end module fortweb_static
