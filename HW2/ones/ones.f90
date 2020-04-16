program ones
!----------------------------------------------------------------------
! Programmer: Bryan Garcia
! Course: AM 250 - Introduction to High Performance Computing
! Description:
!   Dyanamically creates a square array of a size input by the user
!   at runtime. 
!   Assigns certain entries in the array to be 1's and the rest 0's
!----------------------------------------------------------------------
use ones_helper
implicit none

    integer :: ndim
    integer, allocatable :: onesArr(:,:), newOnesArr(:,:)


    write (*,*) "Enter the dimension of your new Ones square-array: "
    read *, ndim
    
    call make_ones( onesArr, ndim)
    call compute_ones( onesArr, newOnesArr)
    
    print *, '-----------------------------------'
    call print_ones( onesArr )
    print *, '-----------------------------------'
    
    print *, '-----------------------------------'
    call print_ones( newOnesArr )
    print *, '-----------------------------------'

    deallocate( onesArr )
    deallocate( newOnesArr )

end program ones