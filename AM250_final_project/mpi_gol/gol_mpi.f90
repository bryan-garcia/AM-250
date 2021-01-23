!------------------------------------------------------------------------------
!   gol_mpi.f90
!
!   Programmer: Bryan Garcia
!   Course: AM 250
!   Description: MPI implementation of game of life with column-wise domain
!   decomposition.
!
!------------------------------------------------------------------------------
program gol_mpi
use mpi

! My helper module containing key subroutines.
use gol_helper

implicit none

! MPI related constants (IDs, number of processors, etc)
integer pid, num_procs, ierr, request, status(mpi_status_size)

! Arrays containing the local partition's state
logical, dimension(:, :), allocatable :: local_grid

! Column height and number of columns
integer :: col_height, num_cols, row_width, num_rows

! Lookup structure for telling each processor how many columns to operate on
integer, dimension(:), allocatable :: workload_lookup

! Configuration flag corresponding to what to initialize a local grid to
integer :: config, random = 0, glider = 1, lw_spaceship = 2, exploder = 3, empty = 4

! Looping variables
integer :: steps, i, j, multiples, reps

! Character to be printed to screen representing "alive" cells
character :: alive_icon, dead_icon

! Flag for printing ghost cells
logical :: show_ghosts

! Local array containing processor ID, "left and right" processors, and # of processors
integer, dimension(4) :: pid_buffer

! Timing variables
real(kind=8) :: start, end
real(kind=8), dimension(:), allocatable :: t_data, t_buff


!-----------------------------------------------
! Get MPI up and running!
!-----------------------------------------------
call mpi_init(ierr)
call mpi_comm_rank(mpi_comm_world, pid, ierr)
call mpi_comm_size(mpi_comm_world, num_procs, ierr)

!-----------------------------------------------
! Setup of grid size and determine the workload
! per processor.
!-----------------------------------------------


! Controls grid size (edit me if you'd like!)
col_height = 20
row_width = col_height




! Allocate some memory for each processor to access their workload
allocate(workload_lookup(num_procs))

! Have pid 0 compute workload
if(pid .eq. 0) then
    call calculate_workload(col_height, row_width, workload_lookup, num_procs)
end if

! Broadcast workload per processor to each processors lookup buffer
call mpi_bcast(workload_lookup, num_procs, mpi_int, 0, mpi_comm_world, ierr)

! Grid size with 2 extra entries (for ghost entries)
num_cols = workload_lookup(pid + 1) + 2

! Allocate the local grid! (remember num_cols already has +2 for ghosts)
allocate(local_grid(col_height + 2, num_cols))

! No longer need workload buffer
deallocate(workload_lookup)




! Character to print to screen for cells (edit me if you'd like!)
alive_icon = "o"
dead_icon = "_"

! Ghost chell print flag
show_ghosts = .false.








!-----------------------------------------------
! Initialize grid to some kind of configuration
! and determine which processor communicates
! with which.
!
!   - Project requirements want to see a glider
!     traverse the partitioned domain.
!   - Config options:
!     glider, random, or empty
!
!-----------------------------------------------
if (pid .eq. 0) then
    config = glider
else
    config = empty
end if

! Establishes ring-like communication pattern
call establish_communication(pid, pid_buffer, num_procs)

! Actually initializes the grid to the configuration
call initialize_grid_mpi(local_grid, pid_buffer, config)

! Print out the initial state (all processors)

print *, "# Iteration ", 0
call print_grid(local_grid, alive_icon, dead_icon, pid_buffer(1), show_ghosts)

do steps = 1, 80

    ! Update the state of the grid according to GOL rules.
    call update_grid(local_grid, pid_buffer)

    ! Print out the updated state if step is a multiple of 20 (per project requirements).
    if (mod(steps, 20) .eq. 0) then
        print *, "# Iteration ", steps
        call print_grid(local_grid, alive_icon, dead_icon, pid_buffer(1), show_ghosts)
    end if

end do

! Done.

deallocate(local_grid)


! End MPI usage.
call mpi_finalize(ierr)
stop

end program gol_mpi
