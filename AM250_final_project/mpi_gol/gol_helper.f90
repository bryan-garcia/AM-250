module gol_helper
!------------------------------------------------------------------------------
!   gol_helper.f90
!
!   Programmer: Bryan Garcia
!   Course: AM 250
!   Description: Helper module for 'gol_mpi.f90' 
!           *(developed for column-wise domain decomposition)*
!           *(Assumes N x N grid)*
!
!------------------------------------------------------------------------------
use mpi
use, intrinsic :: ISO_C_BINDING

INTEGER :: GNUM_PROCS

contains 
subroutine calculate_workload(height, width, work_buffer, num_procs)
!------------------------------------------------------------------------------
!   calculate_workload
!
!   Description: Determines how many columns to distribute among processors
!
!   Parameters:
!       height : integer
!           Height of columns
!
!       work_buffer : integer array
!           "Lookup" data structure to contain workload per processor
!
!       num_procs : integer
!           Number of processors available
!
!------------------------------------------------------------------------------
    implicit none
    integer :: height, width, num_procs
    integer :: work_buffer(:)

    ! If more processors than columns
    if (num_procs .gt. width) then

        ! Assign a single column per processor...
        work_buffer(1:num_procs-height) = 1

        ! ...Up to max processor ID
        work_buffer(num_procs-height+1:) = 0 

    ! If number of columns equals number of processors
    else if (num_procs .eq. width) then

        ! Assign one to each.
        work_buffer(1:) = 1

    ! If more columns than processors
    else 

        ! Attempt to evenly distrubute columns across processors

        ! Equal load balancing when mod(height, num_procs) .eq. 0

        ! PID 0 will get the "spillage"
        work_buffer(1) = height / num_procs + mod(height, num_procs)

        ! While all others get an equal number
        work_buffer(2:) = height / num_procs
    end if

    GNUM_PROCS = num_procs

end subroutine calculate_workload

subroutine establish_communication(pid, pid_buffer, num_procs)
!------------------------------------------------------------------------------
!   establish_communication
!
!   Description: Determines which processors to communicate with.
!
!   Parameters:
!       pid : integer
!           Calling processors ID
!
!       pid_buffer : integer array
!           Array containing communication data
!
!           pid_buffer(1) : current processor
!           pid_buffer(2) : processor to "left"
!           pid_buffer(3) : processor to "right"
!           pid_buffer(4) : total number of processors
!
!       num_procs : integer
!           Number of processors available
!
!------------------------------------------------------------------------------
    implicit none
    integer :: pid, num_procs, pid_to_left, pid_to_right
    integer :: pid_buffer(:)


    ! Determine which processors should communicate with who.
    
    ! First processor communicates with last processor
    if (pid .eq. 0) then
        pid_to_left = num_procs - 1
        pid_to_right = pid + 1
    
    ! Last processor communicates with first processor
    else if (pid .eq. num_procs - 1) then
        pid_to_left = pid - 1
        pid_to_right = 0
    
    ! All others communicate to adjacent processors
    else
        pid_to_left = pid - 1
        pid_to_right = pid + 1
    end if

    ! Store values in pid_buffer for future reference
    pid_buffer(1) = pid
    pid_buffer(2) = pid_to_left
    pid_buffer(3) = pid_to_right
    pid_buffer(4) = num_procs

end subroutine establish_communication

subroutine initialize_grid_mpi(in_grid, pid_buffer, config)
!------------------------------------------------------------------------------
!  initialize_grid_mpi
!
!   Description: Initialize a local grid to a particular configuration
!
!   Parameters:
!       in_grid : logical array
!           2D logical array container representing our domain
!
!       pid_buffer : integer array
!           Array containing communication data
!
!           pid_buffer(1) : current processor
!           pid_buffer(2) : processor to "left"
!           pid_buffer(3) : processor to "right"
!           pid_buffer(4) : total number of processors
!
!       config : integer
!           Configuration code.
!
!           config = 0 : Random entries
!           config = 1 : Glider configuration
!           config = 2 : Empty 
!
!------------------------------------------------------------------------------
    implicit none
    logical :: in_grid(:, :)
    logical :: rlogical = .false.
    integer :: i, j, height, width, config, pid, seed
    integer :: pid_buffer(:)

    ! Get the grid dimensions
    height = size(in_grid, 1)
    width = size(in_grid, 2)

    ! Temporary reference to calling processor ID
    pid = pid_buffer(1)

    ! Determine which configuration to initialize to
    select case(config)

        ! Random entries
        case(0)

            seed = time() + pid
            call srand(seed)

            do i = 2, height - 1
                do j = 2, width - 1
                    if (rand() .gt. 0.5) then
                        rlogical = .true.
                    else
                        rlogical = .false.
                    end if
                    in_grid(i, j) = rlogical
                end do
            end do
        
        ! Glider configuration
        case(1)

            ! Glider as close to the upper left corner as possible
            !   Recall: cannot start at index 1 because of ghosts :)
            in_grid(3, 2) = .true.
            in_grid(4, 3) = .true.
            in_grid(2:4, 4) = .true.

            ! Go back over the grid and set the rest to .false.
            ! aka empty entries
            do i = 1, height
                do j = 1, width
                    in_grid(j, i) = .not. xor(in_grid(j, i), .true.)
                end do
            end do

        ! Empty grid
        case(2)
            do i = 1, height
                do j = 1, width
                    in_grid(j, i) = .false.
                end do
            end do

    end select

    ! Time to initialize the ghost cells....
    call set_periodic_boundary(in_grid, pid_buffer)

end subroutine initialize_grid_mpi

subroutine set_periodic_boundary(grid, pid_buffer)
!------------------------------------------------------------------------------
!  set_periodic_boundary
!
!   Description: Set periodic boundary conditions across processors decomposed
!   column-wise.
!
!   Parameters:
!       grid : logical array
!           2D logical array container representing our current domain state
!
!       pid_buffer : integer array
!           Array containing communication data
!
!           pid_buffer(1) : current processor
!           pid_buffer(2) : processor to "left"
!           pid_buffer(3) : processor to "right"
!           pid_buffer(4) : total number of processors
!
!------------------------------------------------------------------------------
    implicit none
    logical :: grid(:, :)
    logical, dimension(:), allocatable :: temp_col(:)
    integer :: height, width
    integer :: pid_buffer(:)
    integer :: request, status(mpi_status_size), ierr

    ! Get our grid dimensions
    height = size(grid, 1)
    width = size(grid, 2)

    ! Allocate a local column buffer to store the data that will be transmitted.
    allocate(temp_col(height - 2))

    ! Get local grid's right-most non-ghost column
    temp_col = grid(2 : height - 1, width - 1)

    ! "No wait" send right-most column to partner processor on right.
    call mpi_isend(temp_col, height - 2, mpi_logical, pid_buffer(3), &
                    123, mpi_comm_world, request, ierr)

    ! Recv right-most column from partner processor on left. 
    !   Store in left ghost column.
    call mpi_recv(grid(2: height-1, 1), height-2, mpi_logical, pid_buffer(2), &
                    123, mpi_comm_world, status, ierr)

    ! Get local grid's left-most non-ghost column
    temp_col = grid(2 : height-1, 2)

    ! "No wait" send left-most column to partner processor on left.
    call mpi_isend(temp_col, height-2, mpi_logical, pid_buffer(2), &
                    123, mpi_comm_world, request, ierr)

    ! Recv left-most column from partner processor on right. 
    !   Store in right ghost column.
    call mpi_recv(grid(2: height-1, width), height, mpi_logical, pid_buffer(3), &
                    123, mpi_comm_world, status, ierr)

    ! No need for the temporary column anymore
    deallocate(temp_col)

    ! Set the top and bottom boundary conditions within a local grid.
    !   Omits the corners for the time being
    grid(1, 2:width-1) = grid(height-1, 2:width-1)
    grid(height, 2:width-1) = grid(2, 2:width-1)


    ! Now we need to set the corners.


    ! Check if we're operating on the global grid's left-most or right-most column.
    !   These will be associated with PID 0 and PID (num_procs - 1)
    if ((pid_buffer(1) .eq. 0) .or. (pid_buffer(1) .eq. pid_buffer(4) - 1)) then

        ! Send and receive ;)
        if (pid_buffer(1) .eq. 0) then

                ! Send the NW non-ghost corner to the SE ghost cell.
                !   Requires a send to PID (num_procs - 1)

                ! Non-blocking send...
                call mpi_isend(grid(2, 2), 1, mpi_logical, pid_buffer(4) - 1, &
                                123, mpi_comm_world, request, ierr)

                ! ...but blocking receive. Gets SE non-ghost corner and stores
                ! in NW ghost cell.
                call mpi_recv(grid(1, 1), 1, mpi_logical, pid_buffer(4) - 1, &
                                123, mpi_comm_world, status, ierr)

                ! Send the SW non-ghost corner to the NE ghost cell.
                !   Again, requires a send to PID (num_procs - 1)
                call mpi_isend(grid(height-1,2), 1, mpi_logical, pid_buffer(4)-1, &
                                123, mpi_comm_world, request, ierr)

                ! Blocking recieve the NE non-ghost cell and store in SW ghost cell
                call mpi_recv(grid(height,1), 1, mpi_logical, pid_buffer(4)-1, &
                                123, mpi_comm_world, status, ierr)

        else


                call mpi_recv(grid(height, width), 1, mpi_logical, 0, &
                                123, mpi_comm_world, status, ierr)
                call mpi_send(grid(height - 1, width - 1), 1, mpi_logical, 0, &
                                123, mpi_comm_world, ierr)
                call mpi_recv(grid(1, width), 1, mpi_logical, 0, &
                                123, mpi_comm_world, status, ierr)
                call mpi_send(grid(2, width - 1), 1, mpi_logical, 0, &
                                123, mpi_comm_world, ierr)
        end if

    else

        ! Set NW corner to SE inner corner
        grid(1,1) = grid(height-1,width-1)

        ! Set NE corner to SW inner corner
        grid(1,width) = grid(height-1,2)

        ! Set SW corner to NE inner corner
        grid(height,1) = grid(2,width-1)

        ! Set SE corner to NW inner corner
        grid(height, width) = grid(2,2)

    end if
    
    call mpi_barrier(mpi_comm_world, ierr)



end subroutine set_periodic_boundary

subroutine update_grid(grid, pid_buffer)
!------------------------------------------------------------------------------
!  update_grid
!
!   Description: Update the state of the grid according to the GOL rules.
!
!   Parameters:
!       grid : logical array
!           2D logical array container representing our current domain state
!
!       pid_buffer : integer array
!           Array containing communication data
!
!           pid_buffer(1) : current processor
!           pid_buffer(2) : processor to "left"
!           pid_buffer(3) : processor to "right"
!           pid_buffer(4) : total number of processors
!
!------------------------------------------------------------------------------
    implicit none
    logical :: grid(:, :)
    integer :: pid_buffer(:)
    logical, dimension(:, :), allocatable :: temp_grid
    integer :: i, j, height, width, counter

    height = size(grid, 1)
    width = size(grid, 2)


    allocate(temp_grid(height, width))
    temp_grid = grid

    ! Remember to only check non-ghost entries
    do i = 2, height - 1
        do j = 2, width - 1

            ! Start a counter...
            counter = 0


            ! Begin checking around cells!
            if (temp_grid(i - 1, j - 1)) then
                counter = counter + 1
            end if
            if (temp_grid(i - 1, j)) then
                counter = counter + 1
            end if
            if (temp_grid(i - 1, j + 1)) then
                counter = counter + 1
            end if
            if (temp_grid(i, j - 1)) then
                counter = counter + 1
            end if
            if (temp_grid(i, j + 1)) then
                counter = counter + 1
            end if
            if (temp_grid(i + 1, j - 1)) then
                counter = counter + 1
            end if
            if (temp_grid(i + 1, j)) then
                counter = counter + 1
            end if
            if (temp_grid(i + 1, j + 1)) then
                counter = counter + 1
            end if

            ! Finally determine if alive or dead.
            if (counter .eq. 3) then
                grid(i, j) = .true.
            else if (counter .ne. 2) then
                grid(i, j) = .false.
            end if
        end do
    end do

    ! Now update the boundary conditions
    call set_periodic_boundary(grid, pid_buffer)

    deallocate(temp_grid)

end subroutine update_grid

subroutine print_grid(in_grid, alive_char, dead_char, pid, include_ghosts)
!------------------------------------------------------------------------------
!   print_grid
!
!   Description: Print out the state of the grid
!
!   Parameters:
!       in_grid : logical array
!           2D logical array container representing our current domain state
!
!       alive_char : character
!           Character to be printed representing alive cells
!
!       dead_char : character
!           Character to be printed representing dead cells
!
!       pid : integer
!           Processor ID of caller. Will print out as well.
!
!       include_ghosts : logical
!           Boolean flag for determining whether to print out ghost cells
!           as well.
!------------------------------------------------------------------------------
    implicit none
    logical :: in_grid(:, :)
    logical :: include_ghosts
    integer :: i, j, r_start, r_end, c_start, c_end, rows, cols, pid
    character :: alive_char, dead_char

    rows = size(in_grid, 1)
    cols = size(in_grid, 2)

    if (include_ghosts) then
        r_start = 1
        c_start = 1
        r_end = rows
        c_end = cols
    else
        r_start = 2
        c_start = 2
        r_end = rows - 1
        c_end = cols - 1
    end if

    print *, "PID #", pid, " has this grid config:"
    do i = r_start, r_end
        do j = c_start, c_end
            if (in_grid(i, j) .eqv. .true.) then
                write(*, "(A)", advance = "NO") alive_char
            else
                write(*, "(A)", advance = "NO") dead_char
            end if
        end do 
        write(*, *)
    end do
    write(*, *)

end subroutine print_grid

subroutine global_print_grid(pid, in_grid, &
                             global_nrows, global_ncols, alive_char, dead_char)
        implicit none
        logical, target :: in_grid(:, :)
        logical, dimension(:), allocatable, target :: global_grid_buff(:)
        logical, pointer :: temp_buff(:, :)
        integer :: i, j, r_start, r_end, c_start, c_end, local_nrows, local_ncols, global_nrows, global_ncols, pid
        integer, dimension(2) :: shape_buffer
        INTEGER, DIMENSION(:), ALLOCATABLE :: recvcounts(:), dipls(:)
        character :: alive_char, dead_char
        integer :: status(mpi_status_size), ierr
    

        ! Get size of local processor's data.
        local_nrows = size(in_grid, 1)
        local_ncols = size(in_grid, 2)

        ! Set up gatherv buffer that maps number of counts to recv for each pid.
        ALLOCATE(recvcounts(GNUM_PROCS))
        ALLOCATE(dipls(GNUM_PROCS))

        do i = 1, GNUM_PROCS
            recvcounts(i) = (local_ncols) * (local_nrows)  ! pid is zero-based so add 1.
            dipls(i) = (i - 1) * (local_nrows) * (local_ncols-2)
        end do

        if (pid .eq. 0) then
            ! Master allocate grid to print. NO GHOST CELLS
            ALLOCATE(global_grid_buff(global_nrows * global_ncols))
        end if

        call MPI_Gatherv(in_grid, local_nrows * local_ncols, mpi_logical, &
                         global_grid_buff, recvcounts, dipls, mpi_logical, 0, mpi_comm_world, ierr)
        DEALLOCATE(recvcounts)
        DEALLOCATE(dipls)
        ! Printing information: show ghost cells or not.
    
       if (pid .eq. 0) then
            shape_buffer(1) = global_nrows
            shape_buffer(2) = global_ncols
            call C_F_POINTER (C_LOC(global_grid_buff), temp_buff, shape_buffer)

            do i = 2, global_nrows-1
                do j = 2, global_ncols-1
                    if (temp_buff(i, j) .eqv. .true.) then
                        write(*, "(A)", advance = "NO") alive_char
                    else
                        write(*, "(A)", advance = "NO") dead_char
                    end if
                end do 
                write(*, *)
            end do
            write(*, *)

            DEALLOCATE(global_grid_buff)
       end if

       call mpi_barrier(mpi_comm_world, ierr)

end subroutine global_print_grid


subroutine clear_screen()
!------------------------------------------------------------------------------
!   clear_screen
!
!   Description: Clear terminal (used this in my serial implementation).
!
!------------------------------------------------------------------------------
    print *, achar(27)//"[2J"

end subroutine clear_screen


end module gol_helper
