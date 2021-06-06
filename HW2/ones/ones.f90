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
use, intrinsic :: ISO_C_BINDING
implicit none

    integer :: ndim
    integer :: shape_buff (1)
    integer, allocatable, target :: onesArr(:,:)
    INTEGER, ALLOCATABLE :: newOnesArr(:,:)
    integer, pointer :: reshaped_arr(:)

    INTEGER, DIMENSION(3) :: a
    INTEGER, DIMENSION(5) :: b

    a = (/1, 2, 3/)
    b = (/9,8,7,6,5/)

    b(:3) = a(:)

    write (*,*) "Enter the dimension of your new Ones square-array: "
    read *, ndim

    shape_buff(1) = ndim * ndim
    
    call make_ones( onesArr, ndim)
    call compute_ones( onesArr, newOnesArr)
    
    print *, '-----------------------------------'
    call print_ones( onesArr )
    print *, '-----------------------------------'
    
    print *, '-----------------------------------'
    call print_ones( newOnesArr )
    print *, '-----------------------------------'


    print *, shape(onesArr)
    print *, shape(reshape(onesArr, shape_buff))
    print *, shape(onesArr)

    call C_F_POINTER (C_LOC(onesArr), reshaped_arr, shape_buff)

    print *, reshaped_arr(:)

    deallocate( onesArr )
    deallocate( newOnesArr )

end program ones