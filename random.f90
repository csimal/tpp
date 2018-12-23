module random
    use constants
    implicit none
    contains

! PRE: /
! POST: returns a pseudorandom number.
! Description: This function returns pseudorandom numbers using a
! congruential linear generator. The numbers generated follow a
! discrete uniform distribution on the integers from 0 to 2^31 -1
! Use of this function should be avoided, as it fails proper
! randomness tests.
function randu(seed) result(res)
    integer, intent(in), optional :: seed
    integer(kind=8), save :: r
    integer(kind=8) :: res
    integer(kind=8) :: m = 2**31
    integer(kind=8) :: a = 65539
    logical, save :: init = .true.
    if (init) then
        call system_clock(r)
        init = .false.
    end if
    if (present(seed)) then
        r = seed
    end if

    res = modulo(a*r,m)
    r = res
end function randu

! PRE: /
! POST: returns a pseudorandom number.
! Description: This function returns pseudorandom numbers using the xorshift
! algorithm. An optional seed parameter can be provided, otherwise the seed
! is set using the current system clock time upon the first call of the
! function. If the seed is somehow 0, a magic number is used to ensure
! a non-zero seed.
function random_xorshift(seed) result(res)
    integer(kind=8), intent(in), optional :: seed
    integer(kind=8), save :: x
    integer(kind=8) :: res, a1=21, a2=35, a3=4
    logical, save :: init = .true.

    if (init) then
        call system_clock(x)
        init = .false.
    end if
    if (present(seed)) then
        x = seed
    end if
    if (x.eq.0) then
        x = 42 ! magic number
    end if
    res = ieor(x,rshift(x,a1))
    res = ieor(res,lshift(res,a2))
    res = ieor(res,rshift(res,a3))
    x = res
end function random_xorshift

! PRE: /
! POST: returns a pseudorandom number. The numbers generated follow
! a discrete uniform distribution on the integers from 0 to 2^62 -1
function ranq1() result(res)
    integer :: x
    integer :: res, m=2**62, a=2685821657736338717
    x = random_xorshift()
    res = modulo(a*x, m)
end function ranq1

! PRE: /
! POST: returns a pseudorandom number. The numbers generated follow
! a uniform distribution on the interval [0,1]
function random_uniform() result(res)
    real(dp) :: res
    res = real(ranq1(),dp)/real(2**62 -1,dp)
end function random_uniform

 end module random
