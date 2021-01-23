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

! custom MPI groups and comms
integer temp_group, my_group, my_comm

! MPI request handles.
integer pingSend, pongSend, pingRecv, pongRecv, temp

! MPI status handles.
integer pongStatus(mpi_status_size), pingStatus(mpi_status_size)

! Processor IDs to play ping-pong.
integer ping, pong

integer, dimension(2) :: ranks

! Ping-pong game rules & message length.
integer hits, hits2win, mlen, i, j, u1

! For getting latency time
real(kind=8) start, end, startup, endup

real(kind=8), dimension(:), allocatable :: time_buff, time_dat
character, dimension(:), allocatable :: ping_message, pong_message

! Get MPI up and running.
call cpu_time(startup)
call mpi_init(ierr)
call mpi_comm_rank(mpi_comm_world, pid, ierr)
call mpi_comm_size(mpi_comm_world, numprocs, ierr)
call cpu_time(endup)

print *, pid, endup - startup

u1 = 11

! Set some Ping-Pong rules
hits2win = 1000
hits = 1
mlen = 100

! Processors to  play Ping-Pong
ping = 0
pong = 3

ranks(1) = ping
ranks(2) = pong

call mpi_comm_group(mpi_comm_world, temp_group, ierr)
call mpi_group_incl(temp_group, 2, ranks, my_group, ierr)
call mpi_comm_create(mpi_comm_world, my_group, my_comm, ierr)

! If processor is ping or pong...
if ((pid .eq. ping) .or. (pid .eq. pong)) then

    i = 1
    allocate(time_dat(mlen))
    do while (i .le. mlen)

        allocate(ping_message(i))
        allocate(time_buff(hits2win))

        j = 1
        do while (j .le. i)
            ping_message(j) = "a"
            j = j + 1
        end do

        call mpi_barrier(my_comm, ierr)

        ! Play some ping-pong...
        do while (hits .le. hits2win)

            ! If you're ping:
            if (pid .eq. ping) then

                ! Send message to pong.
                call cpu_time(start)
                call mpi_isend(ping_message, i, mpi_char, pong, 123, mpi_comm_world, pingSend, ierr)

                ! Wait for sending to complete.
                call mpi_wait(pingSend, pingStatus, ierr)

                !print *, "Ping sent a message!"
                call mpi_recv(end, 1, mpi_double, pong, 123, mpi_comm_world, temp, ierr)
                

                ! Increase hit tally.
                hits = hits + 1
                
                ! Start listening for a message from pong.
                call mpi_irecv(ping_message, i, mpi_char, pong, 123, mpi_comm_world, pingRecv, ierr)

                ! Wait for message from pong to be fully received.
                call mpi_wait(pingRecv, pingStatus, ierr)

                time_buff(hits) = end - start
                !print *, "Ping got a message: '", ping_message, "'"

            ! Else you're pong:
            else

                ! Start listening for a message from ping.
                call mpi_irecv(ping_message, i, mpi_char, ping, 123, mpi_comm_world, pongRecv, ierr)

                ! Wait for message from ping to be fully received.
                call mpi_wait(pongRecv, pongStatus, ierr)
                call cpu_time(end)
                !print *, "Pong got a message: '", pong_message, "'"

                call mpi_send(end, 1, mpi_double, ping, 123, mpi_comm_world, ierr)
                

                ! Send message to ping.
                
                call mpi_isend(ping_message, i, mpi_char, ping, 123, mpi_comm_world, pongSend, ierr)
                ! Wait for sending to complete.
                call mpi_wait(pongSend, pongStatus, ierr)
                
                !print *, "Pong sent a message!"
                
                ! Increase hit tally.
                hits = hits + 1

            end if

        end do

        if (pid .eq. ping) then
            time_dat(i) = (sum(time_buff)/size(time_buff)) * 1.0_8
            print *, i, " time: ", time_dat(i)
            print *, "startup: ", endup - startup
        end if

        call mpi_barrier(my_comm, ierr)

        deallocate(time_buff)
        deallocate(ping_message)

        i = i + 1
        hits = 0

    end do

end if

    if (pid .eq. 0) then
        print *, time_dat
        open(unit=u1, file='benchmark.dat', form='unformatted', access="stream")
        write(u1) time_dat
        close(u1)
    end if

    call mpi_barrier(mpi_comm_world, ierr)

    deallocate(time_dat)

! End MPI usage.
call mpi_finalize(ierr)
stop

end program ping_mpi