program ssr_mpi
!----------------------------------------------------------------------
! Programmer: Bryan Garcia
! Course: AM 250 - Introduction to High Performance Computing
! Description:
!   A 's'imple 's'end 'r'ecieve MPI program.
!
!----------------------------------------------------------------------
use mpi
implicit none

! MPI related constants.
integer ierr, pid, numprocs, stat(mpi_status_size)

! Send ID and receive ID respectively.
integer sid, rid

! Real array of data to send
real, dimension(5) :: rarr
integer i, dim

sid = 1
rid = 2
dim = 5

rarr = (/1.0, 2.0, 3.0, 4.0, 5.0/)

! Get MPI up and running.
call mpi_init(ierr)
call mpi_comm_rank(mpi_comm_world, pid, ierr)
call mpi_comm_size(mpi_comm_world, numprocs, ierr)

! If designated send ID...
if (pid .eq. sid) then
    print *, "Sending modified array data from processor ", sid, " to processor ", rid
    do i = 1, dim 
        rarr(i) = 2.0 * rarr(i)
    end do

    print *, "Here's what I'm sending: ", rarr(:)
    call mpi_send(rarr, dim, mpi_real, rid, 123, mpi_comm_world, ierr)
end if

! If designated receive ID...
if (pid .eq. rid) then
    print *, "Receiving modified array data from processor ", sid
    print *, "First, check out my buffer:", rarr(:)

    call mpi_recv(rarr, dim, mpi_real, sid, 123, mpi_comm_world, stat, ierr)

    print *, "Now I have:", rarr(:)
end if

! End MPI usage.
call mpi_finalize(ierr)
stop

end program ssr_mpi