module ipf
    use constants
    use random

! """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
! File: ipf.f90
! Author: csimal
! Description: This module handles population generation using ipf and trs
! methods
! """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

    implicit none
    contains

! PRE: /
! POST: cont_out contains the result of a single iteration of ipf on cont_in
! Description: ipf_iter computes a single iteration of ipf of the contigency table cont_in
! returning the new table in cont_out
! Arguments:
! IN : cont_in 
! OUT : cont_out
! IN : c_gen contains the age group contraints for the population
! IN : c_sex contains the gender contraints for the population
! IN : c_dipl contains the diploma constraints for the population
! IN : c_stat contains the work status contraints for the population
! OUT : d_gen contains the marginal distribution for the age group variable
! OUT : d_sex contains the marginal distribution for the gender variable
! OUT : d_dipl contains the marginal distribution for the diploma variable
! OUT : d_stat contains the marginal distribution for the work status
subroutine ipf_iter(cont_in, cont_out, c_gen, c_sex, c_dipl, c_stat, d_gen,d_sex,d_dipl,d_stat)
    real(dp), dimension(20,2,8,3), intent(in) :: cont_in
    real(dp), dimension(20,2,8,3), intent(out) :: cont_out
    integer, dimension(20), intent(in) :: c_gen
    integer, dimension(2), intent(in) :: c_sex
    integer, dimension(8), intent(in) :: c_dipl
    integer, dimension(3), intent(in) :: c_stat
    real(dp), dimension(20), intent(out) :: d_gen
    real(dp), dimension(2), intent(out) :: d_sex
    real(dp), dimension(8), intent(out) :: d_dipl
    real(dp), dimension(3), intent(out) :: d_stat
    integer :: i

    cont_out = cont_in

    d_sex = sum(sum(sum(cont_out,4),3),1)
    do i = 1,2,1
        cont_out(:,i,:,:) = cont_out(:,i,:,:)*(c_sex(i)/d_sex(i))
    end do
    d_gen = sum(sum(sum(cont_out,4),3),2)
    do i = 1,20,1
        cont_out(i,:,:,:) = cont_out(i,:,:,:)*(c_gen(i)/d_gen(i))
    end do
    d_dipl = sum(sum(sum(cont_out,4),2),1)
    do i = 1,8,1
        cont_out(:,:,i,:) = cont_out(:,:,i,:)*(c_dipl(i)/d_dipl(i))
    end do
    d_stat = sum(sum(sum(cont_out,3),2),1)
    do i = 1,3,1
        cont_out(:,:,:,i) = cont_out(:,:,:,i)*(c_stat(i)/d_stat(i))
    end do
end subroutine ipf_iter

! PRE: /
! POST: returns distance between t1 and t2
! Description: dist_cont returns the distance between contingency tables t1 and t2
! to be used as the halting criterion for ipf_gen
! Arguments:
! IN : t1
! IN : t2
function dist_cont(t1, t2) result(d)
    real(dp), dimension(20,2,8,3), intent(in) :: t1, t2
    real(dp) :: d
    d = sum(abs(t1-t2))
end function dist_cont

! PRE: eps > 0, if provided, path must point to an existing directory
! POST:
! Description: ipf_gen iterates ipf_iter on contigency table cont_in, until the distance
! between consecutive iteration is less than eps.
! If the optional 5 digit code ins is provided, it will create a file named
! erreur_ipf_<ins>.txt containing the successive distances between consecutive
! iterations, and a file named margin_<ins>.txt containing the marginal distributions at each iteration
! If the optional argument path is provided, the files are written in the directory it points to
! Arguments:
! IN : cont_in
! IN : c_gen contains the age group contraints for the population
! IN : c_sex contains the gender contraints for the population
! IN : c_dipl contains the diploma constraints for the population
! IN : c_stat contains the work status contraints for the population
! IN : eps is the error tolerance for the ipf
! IN : ins
! IN : path is the optional path where the file should be created
function ipf_gen(cont_in, c_gen, c_sex, c_dipl, c_stat, eps, ins, path) result(ct2)
    real(dp), dimension(20,2,8,3), intent(in) :: cont_in
    real(dp), dimension(20,2,8,3) :: ct1, ct2
    integer, dimension(20), intent(in) :: c_gen
    integer, dimension(2), intent(in) :: c_sex
    integer, dimension(8), intent(in) :: c_dipl
    integer, dimension(3), intent(in) :: c_stat
    real(dp), dimension(20) :: d_gen
    real(dp), dimension(2) :: d_sex
    real(dp), dimension(8) :: d_dipl
    real(dp), dimension(3) :: d_stat
    real(dp), intent(in) :: eps
    integer, intent(in), optional :: ins
    character(len=4), intent(in), optional :: path
    character(len=40) :: filename, filename_margin
    integer :: i, n

    if (present(ins)) then
        write(filename, "(A,I5,A)") "stopping_criteria_", ins, ".txt"
        write(filename_margin, "(A,I5,A)") "margin_", ins, ".txt"
        if (present(path)) then
            filename=adjustl(filename)
            filename = trim(adjustl(path))//adjustl(filename) ! trim to remove trailing whitespace
            filename_margin=adjustl(filename_margin)
            filename_margin = trim(adjustl(path))//adjustl(filename_margin)
        end if
        open(unit=10,file=filename,action="write")
        open(unit=11,file=filename_margin,action="write")
    end if

    ct1 = cont_in
    call ipf_iter(ct1, ct2, c_gen, c_sex, c_dipl, c_stat, d_gen, d_sex, d_dipl, d_stat)

    n = 0

    if (present(ins)) then
        write(10,'(I3,A1,F23.14)') n,' ', dist_cont(ct1,ct2)
        write(11, "(I1)") n
        do i = 1,20, 1
            write(11,"(F11.4,X)",advance='no') d_gen(i)
        end do
        write(11,*) ''
        do i = 1,2, 1
            write(11,"(F13.4,X)",advance='no') d_sex(i)
        end do
        write(11,*) ''
        do i = 1,8, 1
            write(11,"(F11.4,X)",advance='no') d_dipl(i)
        end do
        write(11,*) ''
        do i = 1,3, 1
            write(11,"(F11.4,X)",advance='no') d_stat(i)
        end do
        write(11,*) ''
    end if

    do while (dist_cont(ct1,ct2)>eps)
        ct1 = ct2
        call ipf_iter(ct1, ct2, c_gen, c_sex, c_dipl, c_stat, d_gen, d_sex, d_dipl, d_stat)
        if (present(ins)) then
            n = n+1
            write(10,'(I3,A1,F23.14)') n,' ', dist_cont(ct1,ct2)
            write(11, "(I2)") n
            do i = 1,20, 1
                write(11,"(F11.4,X)",advance='no') d_gen(i)
            end do
            write(11,*) ''
            do i = 1,2, 1
                write(11,"(F13.4,X)",advance='no') d_sex(i)
            end do
            write(11,*) ''
            do i = 1,8, 1
                write(11,"(F11.4,X)",advance='no') d_dipl(i)
            end do
            write(11,*) ''
            do i = 1,3, 1
                write(11,"(F11.4,X)",advance='no') d_stat(i)
            end do
            write(11,*) ''
        end if
    end do

    if (present(ins)) then
        close(10)
        close(11)
    end if
end function ipf_gen

! PRE: /
! POST: all elements of cont_table have their fractional part equal to 0
! Description: trs uses the truncate-replicate-sample method to
! integerize cont_table
! Arguments:
! INOUT : cont_table
subroutine trs(cont_table)
    real(dp), dimension(20,2,8,3), intent(inout) :: cont_table
    real(dp), dimension(20,2,8,3) :: frac_table
    real(dp), dimension(20*2*8*3) :: frac_vector
    integer :: i,j,k,l,m,n,p
    real(dp) :: r, acc, w

    frac_table = fraction(cont_table)
    cont_table = floor(cont_table)
    frac_vector = reshape(frac_table, (/20*2*8*3/))
    n = int(sum(frac_table)) ! the number of people to sample
    do m = 1,n
        w = sum(frac_vector)
        r = random_uniform()*w
        acc = 0.0
        p=1
        do while ( r >= acc+frac_vector(p) .and. p<=20*2*8*3 )
            acc = acc + frac_vector(p)
            p=p+1
        end do
        l = (p-1)/(20*2*8) + 1
        k = modulo(p-1,20*2*8)/(20*2) + 1
        j = modulo(p-1,20*2)/20 + 1
        i = modulo(p-1,20) + 1

        cont_table(i,j,k,l) = cont_table(i,j,k,l) + 1
        frac_vector(p) = 0.0
    end do
end subroutine trs

end module ipf
