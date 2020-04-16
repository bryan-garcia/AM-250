module ones_helper
!----------------------------------------------------------------------
! Programmer: Bryan Garcia
! Description:
!   Helper module for ``ones.f90`` code. Implements key subroutines.
!----------------------------------------------------------------------
contains

    subroutine make_ones(arr, ndim)
    ! -----------------------------------------------------------------
    ! 'make_ones'
    ! 
    !   Description: Dynamically allocate a square array consisting of
    !   psuedo-random 0, 1 entries.
    !
    ! -----------------------------------------------------------------
        implicit none
        integer :: i, j, seed_1, seed_2, rval, ndim
        integer, allocatable :: arr(:,:)

            allocate( arr(ndim, ndim) )
            do i = 1, ndim
                do j = 1, ndim
                    rval = irand() / irand()
                    if (rval .GE. 1) then
                        arr(i, j) = 1
                    else
                        arr(i, j) = 0
                    end if
                end do
            end do

    end subroutine make_ones

    subroutine print_ones(arr)
    ! -----------------------------------------------------------------
    ! 'print_ones'
    ! 
    !   Description: Pretty-print an array (print each row).
    !
    ! -----------------------------------------------------------------
        implicit none
        integer :: arr(:,:)
        integer :: i

            do i = 1, size( arr, 1 )
                print *, arr(i, :)
            end do

    end subroutine print_ones

    subroutine compute_ones(in_arr, out_arr)
    ! -----------------------------------------------------------------
    ! 'compute_ones'
    ! 
    !   Description: Calculates another array that contains a 1 at any 
    !   location if exactly 3 of the 8 surrounding neighboring entries
    !   in the original array contain 1’s. 
    !
    ! -----------------------------------------------------------------
        implicit none
        integer :: in_arr(:,:)
        integer, allocatable :: out_arr(:,:)
        integer :: i, j, ndim, tally

        ! Determine limits of do-loop.
        ndim = size( in_arr, 1)
        allocate( out_arr(ndim, ndim) )

        do i = 1, ndim
            do j = 1, ndim

                ! Initialize tally count to zero.
                tally = 0

                !------------------------------------------------------
                ! First column of the grid.
                !------------------------------------------------------
                if (j .eq. 1) then
                    ! -------------------------------------------------
                    ! Checking if at upper-left or bottom-left corner.
                    ! -------------------------------------------------

                    ! -------------------------------------------------
                    ! Upper-left corner check.
                    ! -------------------------------------------------
                    if (i .eq. 1) then
                        if ( ( in_arr( i, j + 1 ) .eq. 1 ) .AND. &
                            ( in_arr( i + 1, j ) .eq. 1 ) .AND. &
                                ( in_arr( i + 1, j + 1 ) .eq. 1 ) ) then
                                    ! Found exactly three ones.
                                    tally = 3
                        end if
                    ! -------------------------------------------------
                    ! Bottom-left corner check.
                    ! -------------------------------------------------
                    else if (i .eq. ndim) then
                        if ( ( in_arr( i, j + 1 ) .eq. 1 ) .AND. &
                            ( in_arr( i - 1, j ) .eq. 1 ) .AND. &
                                ( in_arr( i - 1, j + 1 ) .eq. 1 ) ) then
                                    ! Found exactly three ones.
                                    tally = 3
                        end if
                    ! -------------------------------------------------
                    ! Arbitrary entry along first column.
                    ! -------------------------------------------------
                    else
                        if ( in_arr( i - 1, j ) .eq. 1 ) then 
                            tally = tally + 1
                        end if
                        if ( in_arr( i - 1, j + 1 ) .eq. 1 ) then
                            tally = tally + 1
                        end if
                        if ( in_arr( i, j + 1 ) .eq. 1 ) then
                            tally = tally + 1
                        end if
                        if ( in_arr( i + 1, j) .eq. 1 ) then
                            tally = tally + 1
                        end if
                        if ( in_arr( i + 1, j + 1 ) .eq. 1 ) then
                            tally = tally + 1
                        end if
                    end if
                !------------------------------------------------------
                ! Last column of the grid.
                !------------------------------------------------------
                else if (j .eq. ndim) then
                    ! -------------------------------------------------
                    ! Checking if at upper-right or bottom-right corner.
                    ! -------------------------------------------------

                    ! -------------------------------------------------
                    ! Upper-right corner check.
                    ! -------------------------------------------------
                    if (i .eq. 1) then
                        if ( ( in_arr( i, j - 1 ) .eq. 1 ) .AND. &
                            ( in_arr( i + 1, j ) .eq. 1 ) .AND. &
                                ( in_arr( i + 1, j - 1 ) .eq. 1 ) ) then
                                    ! Found exactly three ones.
                                    tally = 3
                        end if
                    ! -------------------------------------------------
                    ! Bottom-right corner check.
                    ! -------------------------------------------------
                    else if (i .eq. ndim) then
                        if ( ( in_arr( i, j - 1 ) .eq. 1 ) .AND. &
                            ( in_arr( i - 1, j ) .eq. 1 ) .AND. &
                                ( in_arr( i - 1, j - 1 ) .eq. 1 ) ) then
                                    ! ---------------------------------
                                    ! Found exactly three ones.
                                    ! ---------------------------------
                                    tally = 3
                        end if
                    ! -------------------------------------------------
                    ! Arbitrary entry along last column.
                    ! -------------------------------------------------
                    else
                        if ( in_arr( i - 1, j ) .eq. 1 ) then 
                            tally = tally + 1
                        end if
                        if ( in_arr( i - 1, j - 1 ) .eq. 1 ) then
                            tally = tally + 1
                        end if
                        if ( in_arr( i, j - 1 ) .eq. 1 ) then
                            tally = tally + 1
                        end if
                        if ( in_arr( i + 1, j) .eq. 1 ) then
                            tally = tally + 1
                        end if
                        if ( in_arr( i + 1, j - 1 ) .eq. 1 ) then
                            tally = tally + 1
                        end if
                    end if
                !------------------------------------------------------
                ! Top row of the grid: arbitrary non-corner entry.
                !------------------------------------------------------
                else if (i .eq. 1) then
                    if ( in_arr( i, j -1 ) .eq. 1 ) then 
                        tally = tally + 1
                    end if
                    if ( in_arr( i, j + 1 ) .eq. 1 ) then
                        tally = tally + 1
                    end if
                    if ( in_arr( i + 1, j ) .eq. 1 ) then
                        tally = tally + 1
                    end if
                    if ( in_arr( i + 1, j - 1 ) .eq. 1 ) then
                        tally = tally + 1
                    end if
                    if ( in_arr( i + 1, j + 1 ) .eq. 1 ) then
                        tally = tally + 1
                    end if
                !------------------------------------------------------
                ! Bottom row of the grid: arbitrary non-corner entry.
                !------------------------------------------------------
                else if (i .eq. ndim) then
                    if ( in_arr( i, j -1 ) .eq. 1 ) then 
                        tally = tally + 1
                    end if
                    if ( in_arr( i, j + 1 ) .eq. 1 ) then
                        tally = tally + 1
                    end if
                    if ( in_arr( i - 1, j ) .eq. 1 ) then
                        tally = tally + 1
                    end if
                    if ( in_arr( i - 1, j - 1 ) .eq. 1 ) then
                        tally = tally + 1
                    end if
                    if ( in_arr( i - 1, j + 1 ) .eq. 1 ) then
                        tally = tally + 1
                    end if
                !------------------------------------------------------
                ! Arbitrary non-boundary entry of the grid.
                !------------------------------------------------------
                else
                    if ( in_arr( i, j - 1) .eq. 1 ) then
                        tally = tally + 1
                    end if
                    if ( in_arr( i, j + 1) .eq. 1 ) then
                        tally = tally + 1
                    end if
                    if ( in_arr( i - 1, j - 1) .eq. 1 ) then
                        tally = tally + 1
                    end if
                    if ( in_arr( i - 1, j ) .eq. 1 ) then
                        tally = tally + 1
                    end if
                    if ( in_arr( i - 1, j + 1 ) .eq. 1 ) then
                        tally = tally + 1
                    end if
                    if ( in_arr( i + 1, j - 1 ) .eq. 1 ) then
                        tally = tally + 1
                    end if
                    if ( in_arr( i + 1, j ) .eq. 1 ) then
                        tally = tally + 1
                    end if
                    if ( in_arr( i + 1, j + 1 ) .eq. 1 ) then
                        tally = tally + 1
                    end if
                end if

            !------------------------------------------------------
            ! Determine entry of output grid.
            !------------------------------------------------------
            if (tally .eq. 3) then
                out_arr( i, j ) = 1
            else
                out_arr( i, j ) = 0
            end if

            end do
        end do

    end subroutine compute_ones

end module ones_helper


