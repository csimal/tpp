module constants
! """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
! File: constants
! Author: csimal
! Description: Simple module containing constants used throughout
! the rest of the code. 
! """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    implicit none
    ! the parameter used for double precision floating point numbers
    integer, parameter :: dp = selected_real_kind(15,307)
end module constants
