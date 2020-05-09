program ping_mpi
!----------------------------------------------------------------------
! Programmer: Bryan Garcia
! Course: AM 250 - Introduction to High Performance Computing
! Description:
!   A short little game of ping-pong between processors.
!
!----------------------------------------------------------------------
use mpi
implicit none

! MPI related constants.
integer ierr, pid, numprocs

! MPI request handles.
integer pingSend, pongSend, pingRecv, pongRecv

! MPI status handles.
integer pongStatus(mpi_status_size), pingStatus(mpi_status_size)

! Processor IDs to play ping-pong.
integer ping, pong

! Ping-pong game rules & message length.
integer hits, hits2win, mlen
character message*27

! Get MPI up and running.
call mpi_init(ierr)
call mpi_comm_rank(mpi_comm_world, pid, ierr)
call mpi_comm_size(mpi_comm_world, numprocs, ierr)

! Set some Ping-Pong rules
hits2win = 5
hits = 0

message = "Ping is better at ping-pong"
mlen = 27

! Processors to  play Ping-Pong
ping = 0
pong = numprocs - 1

! If processor is ping or pong...
if ((pid .eq. ping) .or. (pid .eq. pong)) then

    ! Play some ping-pong...
    do while (hits .lt. hits2win)

        ! If you're ping:
        if (pid .eq. ping) then

            ! Send message to pong.
            call mpi_isend(message, mlen, mpi_char, pong, 123, mpi_comm_world, pingSend, ierr)

            ! Wait for sending to complete.
            call mpi_wait(pingSend, pingStatus, ierr)
            print *, "Ping sent a message!"

            ! Increase hit tally.
            hits = hits + 1
            
            ! Start listening for a message from pong.
            call mpi_irecv(message, mlen, mpi_char, pong, 123, mpi_comm_world, pingRecv, ierr)

            ! Wait for message from pong to be fully received.
            call mpi_wait(pingRecv, pingStatus, ierr)
            print *, "Ping got a message: '", message, "'"

            ! Update pong's message.
            message = "Ping is better at ping-pong"

        ! Else you're pong:
        else

            ! Start listening for a message from ping.
            call mpi_irecv(message, mlen, mpi_char, ping, 123, mpi_comm_world, pongRecv, ierr)

            ! Wait for message from ping to be fully received.
            call mpi_wait(pongRecv, pongStatus, ierr)
            print *, "Pong got a message: '", message, "'"

            ! Update ping's message.
            message = "Pong is better at ping-pong"

            ! Send message to ping.
            call mpi_isend(message, mlen, mpi_char, ping, 123, mpi_comm_world, pongSend, ierr)

            ! Wait for sending to complete.
            call mpi_wait(pongSend, pongStatus, ierr)
            print *, "Pong sent a message!"
            
            ! Increase hit tally.
            hits = hits + 1

        end if

    end do

end if

! End MPI usage.
call mpi_finalize(ierr)
stop

end program ping_mpi