program hello_mpi
!----------------------------------------------------------------------
! Programmer: Bryan Garcia
! Course: AM 250 - Introduction to High Performance Computing
! Description:
!   A simple "hello" from each processor.
!
!----------------------------------------------------------------------
use mpi
implicit none

! MPI related constants.
integer ierr, pid, numprocs

! Get MPI up and running.
call mpi_init(ierr)
call mpi_comm_rank(mpi_comm_world, pid, ierr)
call mpi_comm_size(mpi_comm_world, numprocs, ierr)

print *, "Hellooooo! (From processor #", pid, " out of ", numprocs, ")"

! End MPI usage.
call mpi_finalize(ierr)
stop

end program hello_mpi