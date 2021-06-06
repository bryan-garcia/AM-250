program ssr_mpi
!----------------------------------------------------------------------
! Programmer: Bryan Garcia
! Course: AM 250 - Introduction to High Performance Computing
! Description:
!   A 's'imple 's'end 'r'ecieve MPI program.
!
!----------------------------------------------------------------------
use mpi
use, intrinsic :: ISO_C_BINDING
implicit none

! MPI related constants.
integer ierr, pid, numprocs, stat(mpi_status_size)

! Send ID and receive ID respectively.
integer sid, rid

! Real array of data to send
real, dimension(5, 5), target :: rarr
real, pointer :: reshaped_arr(:)

integer i, xdim, ydim
INTEGER, DIMENSION(1) :: buff_len

sid = 1
rid = 2
xdim = 5
ydim = 5
buff_len(1) = xdim * ydim

call RANDOM_NUMBER(rarr)

! Get MPI up and running.
call mpi_init(ierr)
call mpi_comm_rank(mpi_comm_world, pid, ierr)
call mpi_comm_size(mpi_comm_world, numprocs, ierr)

! If designated send ID...
if (pid .eq. sid) then
    print *, "Sending modified array data from processor ", sid, " to processor ", rid
    rarr = 2.0 * rarr

    call C_F_POINTER (C_LOC(rarr), reshaped_arr, buff_len)

    call mpi_send(rarr, xdim * ydim, mpi_real, rid, 123, mpi_comm_world, ierr)
end if

! If designated receive ID...
if (pid .eq. rid) then
    print *, "Receiving modified array data from processor ", sid
    print *, "First, check out my buffer:"
    do i = 1, xdim
        print *, rarr(:, i)
    end do

    call mpi_recv(rarr, xdim * ydim, mpi_real, sid, 123, mpi_comm_world, stat, ierr)

    print *, "Now I have:"
    do i = 1, xdim
        print *, rarr(:, i)
    end do
end if

! End MPI usage.
call mpi_finalize(ierr)
stop

end program ssr_mpi