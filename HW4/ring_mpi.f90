program ring
!----------------------------------------------------------------------
! Programmer: Bryan Garcia
! Course: AM 250 - Introduction to High Performance Computing
! Description:
!   A simple exercise in sending data around in a ring of N processors.
!
!----------------------------------------------------------------------
use mpi
implicit none

! MPI related constants.
integer pid, numprocs, ierr, request, status(mpi_status_size)

! num_shifts: number of shifts to perform.
! LR : pass to left (0) or pass to right (1).
! target_pid : processor to pass data to.
! data : data to pass (pid of a processor).
integer num_shifts, LR, target_pid, data

! Buffer for broadcasting.
integer mybuff(2)

! Get MPI up and running.
call mpi_init(ierr)
call mpi_comm_rank(mpi_comm_world, pid, ierr)
call mpi_comm_size(mpi_comm_world, numprocs, ierr)

! Initialize some data.
num_shifts = 0
data = pid
mybuff = 0

! If you're the root processor...
if (pid .eq. 0) then

    ! Get shifts to perform from user.
    print *, "How many 'shifts' should we do?"
    read(*,*) num_shifts

    ! Get direction from user.
    print *, "Should I pass Left or Right? Enter '0' for Left or '1' for right: "
    read(*,*) LR

    ! Store number of shifts in buffer.
    mybuff(1) = MOD(num_shifts, numprocs)

    ! Store direction in buffer.
    mybuff(2) = LR

end if

! Wait for all processors to get here...
call mpi_barrier(mpi_comm_world, ierr)

! Broadcast num_shifts and LR directino to all processors from root.
call mpi_bcast(mybuff, 2, mpi_int, 0, mpi_comm_world, ierr)

! Get broadcasted data into local num_shifts and LR variables.
num_shifts = mybuff(1)
LR = mybuff(2)

! Calculate a target processor based off num_shifts and direction.
if (LR .eq. 1) then
    target_pid = MOD(pid + num_shifts, numprocs)
else
    target_pid = MOD(pid - num_shifts + numprocs, numprocs)
end if

! Send to your processor and wait for a message from another processor.
call mpi_isend(data, 1, mpi_int, target_pid, 123, mpi_comm_world, request, ierr)
call mpi_recv(data, 1, mpi_int, mpi_any_source, 123, mpi_comm_world, status, ierr)

! Print data you received.
print *, "Processor ", data, " sent data to processor ", pid

! End MPI usage.
call mpi_finalize(ierr)
stop

end program ring