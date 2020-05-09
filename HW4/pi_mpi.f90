program pi_mpi
!----------------------------------------------------------------------
! Programmer: Bryan Garcia
! Course: AM 250 - Introduction to High Performance Computing
! Description:
!   An MPI program to approximate Pi in a Monte Carlo fashion.
!
!----------------------------------------------------------------------
use mpi
implicit none

! MPI related constants.
integer pid, numprocs, ierr, status(mpi_status_size)
integer i, max_iter, seed

! Hit tally.
integer pid_hits

! Buffer to store mpi_gather of hit tallies.
integer, dimension(:), allocatable :: totals
real sgn, dist, pi_approx
real, dimension(2) :: xy
real radius

! Get MPI up and running.
call mpi_init(ierr)
call mpi_comm_rank(mpi_comm_world, pid, ierr)
call mpi_comm_size(mpi_comm_world, numprocs, ierr)

! Allocate buffer size for mpi_gather.
allocate(totals(numprocs))
totals = 0

! max iterations per processor
max_iter = 100000
pid_hits = 0

! Radius of your circle to target.
radius = 1.0

! Generate a random seed for rand()
seed = time() + pid
call srand(seed)

do i = 1, max_iter, 1

    ! Note: rand() generates a real r in (0, 1).
    if (rand() .gt. 0.5) then
        sgn = 1.0
    else 
        sgn = -1.0
    end if

    ! x-coordinate of dart location.
    xy(1) = radius * sgn * rand()

    if (rand() .gt. 0.5) then
        sgn = 1.0
    else 
        sgn = -1.0
    end if

    ! y-coordinate of dart location.
    xy(2) = radius * sgn * rand()

    ! Compute distance of dart location from origin.
    dist = sqrt(xy(1) ** 2 + xy(2) ** 2)

    ! If inside circle, tally!
    if (dist .le. radius) then
        pid_hits = pid_hits + 1
    end if

end do

! Report tallies for each processor.
print *, "Processor ", pid, " got ", pid_hits, " hits in the circle!"

! Have processor 0 gather all hit tallies.
call mpi_gather(pid_hits, 1, mpi_int, totals, 1, mpi_int, 0, mpi_comm_world, ierr)

! If you're processor 0, then compute pi_approx.
if (pid .eq. 0) then

    ! Computing pi approximation.
    pi_approx = 4.0 * (real(sum(totals)) / real(numprocs * max_iter))

    print *, "Approximation of Pi: ", pi_approx

end if

! Deallocate totals buffer.
deallocate(totals)

! End MPI usage.
call mpi_finalize(ierr)
stop

end program pi_mpi