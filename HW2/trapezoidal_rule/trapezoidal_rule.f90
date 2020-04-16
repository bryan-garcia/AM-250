program trapezoidal_integration
! Programmer: Bryan Garcia
! Course: AM 250 - Introduction to High Performance Computing
! Description:
!	Integrates a function f(x) between two limits using the
!	Trapezoidal rule.
implicit none
integer :: N, i
real :: a, b
real :: dx, sum = 0.0
real, external :: f

write (*,*) "------------------------------------"
write (*,*) "Trapezoidal Integration"
write (*,*) "------------------------------------"
write (*,*) NEW_LINE('!')

write (*,*) "Enter the LOWER bounds of integration for f(x): "
read (*,*) a
write (*,*) "Enter the UPPER bounds of integration for f(x): "
read (*,*) b

if (a > b) then
    stop "LOWER bound value greater than UPPER bound value."
end if

write (*,*) "Enter the number of intervals N for integration of f(x): "
read (*,*) N

dx = (b - a) / N

do i = 0, N-1, 1
    sum = sum + ( dx / 2 ) * ( f( a + dx * i ) + f (a + dx * ( i + 1 ) ) )
end do

print *,sum

end program trapezoidal_integration
