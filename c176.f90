program c176

implicit none
integer(16) :: n, i, m, x
integer(16), allocatable :: a(:)

read *, n

if(n == 1) then
 print *, 0
 stop
end if

allocate(a(1:n))

x = 0

read *, a(:)

do i = 1, n-1
 m = a(i) - a(i+1)
 if(m < 0) then
  x = x
 else
  x = x + m
  a(i+1) = a(i+1) + m
 end if
end do

print *, x

end program c176
