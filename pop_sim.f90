module pop_sim
    use constants
    use random
    use list
    implicit none

    contains

        ! PRE: j==1 or j==2, 1 <= i <= 20
        ! POST: age is an integer between 0 and 109
        ! Description: Computes a random age using the probabilities stored in
        ! age_distr. i and j are used to determine which probabilities to use
        ! i is the age bracket (from [0:4] to [95:]), j is the gender
        ! (1 for male, 2 for female)
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

        ! PRE: /
        ! POST: list contains every individual of the population described
        ! in cont_table
        ! Description: generates a list of individuals from cont_table
        ! using age_distr to generate exact ages
        function generate_pop(cont_table, age_distr) result(list)
            real(dp), dimension(20,2,8,3), intent(in) :: cont_table
            real(dp), dimension(22,2,5), intent(in) :: age_distr
            type(ListPerson) :: list
            integer :: i,j,k,l,m
            type(Person), pointer :: p

            do i = 1,20
                do j = 1,2
                    do k = 1,8
                        do l = 1,3
                            do m = 1,int(cont_table(i,j,k,l))
                                call new_person(gen_age(i,j,age_distr),j,k,l,p)
                                call append_person(list,p)
                            end do
                        end do
                    end do
                end do
            end do
        end function generate_pop

        ! PRE: /
        ! POST: baby is associated to a randomly generated newborn
        ! Description: generates a baby of random gender (155 boys for 145 girls)
        subroutine new_baby(baby)
            type(Person), pointer, intent(out) :: baby
            integer :: sex
            if (random_uniform() < 155.0/300.0) then
                sex = 1
            else
                sex = 2
            end if
            call new_person(0,sex,1,1,baby)
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
        subroutine simulate_year(pop,birth_distr,death_distr,n_men,n_women)
            type(ListPerson), intent(inout) :: pop
            real(dp), dimension(35), intent(in) :: birth_distr
            real(dp), dimension(2,105), intent(in) :: death_distr
            integer, intent(out)::n_men,n_women
            type(ListPersonNode), pointer :: node,empty
            type(ListPerson) :: newborns
            type(Person), pointer :: baby
            integer :: age, sex
            n_men=0
            n_women=0
            if (associated(pop%first)) then
                allocate(empty)
                node => empty
                node%right => pop%first

                do while (associated(node%right))
                    node => node%right
                    age = node%person%age
                    sex = node%person%sex
                    if(sex==1) then 
                        n_men=n_men+1
                    else
                        n_women=n_women+1
                    end if
                    if ((sex == 2) .and. (age >= 15) .and. (age <= 49)) then
                        if (random_uniform() < birth_distr(age-15+1)) then
                            call new_baby(baby)
                            if(baby%sex==1)then
                                n_men=n_men+1
                            else
                                n_women=n_women+1
                            end if
                            call append_person(newborns,baby)
                        end if
                    end if
                    node%person%age = age + 1
                    if ( random_uniform() < death_distr(sex,age+1) ) then
                        if (associated(node%left)) then
                            node => node%left
                            call remove_person(pop,node%right)
                        else
                            empty%right => node%right
                            call remove_person(pop,node)
                            node => empty
                        end if
                        if(sex==1) then 
                            n_men=n_men-1
                        else
                            n_women=n_women-1
                        end if
                    end if
                end do

                deallocate(empty)
                call concat_lists(pop,newborns)
            end if

        end subroutine simulate_year

end module pop_sim
