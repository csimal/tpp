module age
    use constants

    implicit none

    contains 

! PRE: "Structure_Age.txt" must be in the same directory as the calling executable
! POST: age_distr contains the age distribution to be used for generating exact
!       ages from age groups
! Description: 
subroutine read_age_distr(age_distr)
    real(dp), dimension(22,2,5), intent(inout) :: age_distr
    integer :: age,i,j
    real(dp) :: s1 = 0.0 , s2 = 0.0
    open (unit = 10, file = "Structure_Age.txt", action="read")
    read(10,*) !skip first line
    do i = 1,22,1
        do j = 1,5,1
            read(10,*) age, age_distr(i,1,j), age_distr(i,2,j)
        end do
        if((i==20) .OR. (i==21) .OR. (i==22)) then 
            s1=s1+sum(age_distr(i,1,:))
            s2=s2+sum(age_distr(i,2,:))
        else
            age_distr(i,1,:)=age_distr(i,1,:)/sum(age_distr(i,1,:))
            age_distr(i,2,:)=age_distr(i,2,:)/sum(age_distr(i,2,:))
        end if
    end do
    do i=20,22,1
        age_distr(i,1,:)=age_distr(i,1,:)/s1
        age_distr(i,2,:)=age_distr(i,2,:)/s2
    end do
    close(10)
end subroutine read_age_distr

! PRE: "Taux_fecondite.txt" must be in the same directory as the calling executable
! POST: birth_distr contains the birth distribution to be used for randomly creating newborns based on age
! Description:
subroutine read_birth_distr(birth_distr)
    real(dp),dimension(35),intent(inout) :: birth_distr
    real(dp) :: x
    integer :: age,i
    open(unit=10, file = "Taux_fecondite.txt",action="read")
    read(10,*) !skip first line
    read(10,*) !skip second line
    do i=1,35,1
        read(10,*) age, x
        birth_distr(i) = x
    end do
    close(10)
end subroutine read_birth_distr

! PRE: "Taux_deces.txt" must be in the same directory as the calling executable
! POST: death_distr contains the death distribution to be used for randomly dying based on age
! Description:
subroutine read_death_distr(death_distr)
    real(dp),dimension(2,105),intent(inout) :: death_distr
    integer age,i
    open(unit=10, file = "Taux_deces.txt",action="read")
    read(10,*) !skip first line
    do i=1,105
        read(10,*) age, death_distr(1,i),death_distr(2,i)
    end do
    close(10)
end subroutine read_death_distr

end module age 



