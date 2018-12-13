module ipf_temp
    implicit none
    contains

 function ipf_iter(ctable, c_gen, c_sex, c_dipl, c_stat) result(ct)
    integer, parameter :: dp = selected_real_kind(15,307)
    real(dp), dimension(20,2,8,3), intent(in) :: ctable
    real(dp), dimension(20,2,8,3) :: ct
    integer, dimension(20), intent(in) :: c_gen
    integer, dimension(2), intent(in) :: c_sex
    integer, dimension(8), intent(in) :: c_dipl
    integer, dimension(3), intent(in) :: c_stat
    integer :: i
    real(dp), dimension(20) :: tot_gen
    real(dp), dimension(2) :: tot_sex
    real(dp), dimension(8) :: tot_dipl
    real(dp), dimension(3) :: tot_stat

    ! ajustement de gen
    tot_gen = sum(sum(sum(ctable,4),3),2)
    do i = 1,20,1
        ct(i,:,:,:) = ctable(i,:,:,:)*(c_gen(i)/tot_gen(i))
    end do
    tot_sex = sum(sum(sum(ctable,4),3),1)
    do i = 1,2,1
        ct(:,i,:,:) = ctable(:,i,:,:)*(c_sex(i)/tot_sex(i))
    end do
    tot_dipl = sum(sum(sum(ctable,4),2),1)
    do i = 1,8,1
        ct(:,:,i,:) = ctable(:,:,i,:)*(c_dipl(i)/tot_dipl(i))
    end do
    tot_stat = sum(sum(sum(ctable,3),2),1)
    do i = 1,3,1
        ct(:,:,:,i) = ctable(:,:,:,i)*(c_stat(i)/tot_stat(i))
    end do
end function ipf_iter

function dist_cont(t1, t2) result(d)
    integer, parameter :: dp = selected_real_kind(15,307)
    real(dp), dimension(20,2,8,3), intent(in) :: t1, t2
    real(dp) :: d
    d = sum(abs(t1-t2))
end function dist_cont       
    
end module ipf_temp
