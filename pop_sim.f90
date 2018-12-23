module pop_sim
    use constants
    use random
    use list

! """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
! File: pop_sim.f90
! Author: meuricel, csimal
! Description: This module handles the simulation on a list of individuals
! """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

    implicit none

    contains

! PRE: j==1 or j==2, 1 <= i <= 20
! POST: age is an integer between 0 and 109
! Description: Computes a random age using the probabilities stored in
! age_distr. i and j are used to determine which probabilities to use
! Arguments:
! IN : i is the age bracket (from [0:4] to [95:])
! IN : j is the gender (1 for male, 2 for female)
! IN : age_distr contains the age distribution per gender and age group (see age.f90)
function gen_age(i,j,age_distr) result(age)
    integer, intent(in) :: i,j
    real(dp), dimension(22,2,5), intent(in) :: age_distr
    integer :: age,m,n
    real(dp) :: r, s

    n = 1
    m = i
    s = 0.0
    r = random_uniform()
    if (i == 20) then
        do while ((r >= s + age_distr(m,j,n)) .and. n<5)
            s = s + age_distr(m,j,n)
            if (n==5 .and. s<r .and. m<23) then
                n = 1
                m = m+1
            else
                n = n+1
            end if
        end do
    else
        do while ((r >= s + age_distr(m,j,n)) .and. n<5)
            s = s + age_distr(i,j,n)
            n = n + 1
        end do
    end if

    age = (m-1)*5 + n-1
end function gen_age

! PRE: list is empty
! POST: list contains every individual of the population described
! in cont_table. sextable contains the number of people per sex and age
! in cont_table
! Description: generates a list of individuals from cont_table
! using age_distr to generate exact ages
! Arguments:
! IN : cont_table is a contingency table describing the population
! IN : age_distr is table containing the age distribution per gender and age group 
! OUT: list is a list of individuals from the population
! OUT: sextable is table containing the number of people per gender and age in the population
subroutine generate_pop(cont_table, age_distr,list,sextable)
    real(dp), dimension(20,2,8,3), intent(in) :: cont_table
    real(dp), dimension(22,2,5), intent(in) :: age_distr
    type(ListPerson),intent(out) :: list
    integer,dimension(2,105),intent(out) :: sextable
    integer :: i,j,k,l,m
    type(Person), pointer :: p

    do i = 1,20
        do j = 1,2
            do k = 1,8
                do l = 1,3
                    do m = 1,int(cont_table(i,j,k,l))
                        call new_person(gen_age(i,j,age_distr),j,k,l,p)
                        sextable(p%sex,p%age+1) = sextable(p%sex,p%age+1)+1
                        call append_person(list,p)
                    end do
                end do
            end do
        end do
    end do
end subroutine generate_pop

! PRE: baby is not allocated
! POST: baby is associated to a randomly generated newborn
! Description: generates a baby of random gender (155 boys for 145 girls)
! Arguments:
! OUT : baby
subroutine new_baby(baby)
    type(Person), pointer, intent(out) :: baby
    integer :: sex
    if (random_uniform() < 155.0/300.0) then
        sex = 1
    else
        sex = 2
    end if
    call new_person(0,sex,1,2,baby)
end subroutine new_baby

! PRE: birth_distr(i) contains the probability of a woman aged 14+i to
! have a child. death_dist(j,i) contains the probability of death at age i
! j==1 for male, 2 for female
! POST: pop is updated with every individual 1 year older
! some people dead, and some newborns
! Description: simulates the passing of a year on the population
! contained in pop. Each individual ages 1 year, and may die and be
! removed from the population. Women aged 15 to 49 may give birth to
! a baby. The babies are appended to the population.
! Arguments:
! IN : pop is a list of individuals
! IN : birth_distr contains the probabilities of a women aged
!      15 to 59 to have a child
! IN : death_distr contains the probabilities per gender and age to die
! INOUT : sextable contains the number of people per gender and age
subroutine simulate_year(pop,birth_distr,death_distr,sextable)
    type(ListPerson), intent(inout) :: pop
    real(dp), dimension(35), intent(in) :: birth_distr
    real(dp), dimension(2,105), intent(in) :: death_distr
    integer,dimension(2,105), intent(inout)::sextable
    type(ListPersonNode), pointer :: node,empty
    type(ListPerson) :: newborns
    type(Person), pointer :: baby
    integer :: age, sex

    if (associated(pop%first)) then
        allocate(empty)
        node => empty
        node%right => pop%first

        do while (associated(node%right))
            node => node%right
            age = node%person%age
            sex = node%person%sex
            if ((sex == 2) .and. (age >= 15) .and. (age <= 49)) then
                if (random_uniform() < birth_distr(age-15+1)) then
                    call new_baby(baby)
                    sextable(baby%sex,1) = sextable(baby%sex,1)+1
                    call append_person(newborns,baby)
                end if
            end if
            node%person%age = age + 1
            sextable(sex,age+1) = sextable(sex,age+1)-1
            sextable(sex,age+2) = sextable(sex,age+2)+1
            if ( random_uniform() < death_distr(sex,age+1) ) then
                if (associated(node%left)) then
                    node => node%left
                    call remove_person(pop,node%right)
                else
                    call remove_person(pop,pop%first)
                    empty%right => pop%first
                    node => empty 
                end if
                sextable(sex,age+2) = sextable(sex,age+2)-1
            end if
        end do
        deallocate(empty)
        call concat_lists(pop,newborns)
    end if
end subroutine simulate_year

end module pop_sim
