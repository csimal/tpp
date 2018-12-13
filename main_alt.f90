program main
    use data_utils
    use constants
    use ipf
    use age
    use list
    use pop_sim

    implicit none
    integer :: total, ins
    integer, dimension(20) :: c_gen
    integer, dimension(2) :: c_sex
    integer, dimension(8) :: c_dipl
    integer, dimension(3) :: c_stat
    real(dp), dimension(20,2,8,3) :: cont, ct
    real(dp) :: eps = 0.000001
    real(dp), dimension(22,2,5) :: age_distr
    type(ListPerson) :: pop
    real(dp), dimension(35) :: birth_distr
    real(dp), dimension(2,105) :: death_distr
    character(len=4) :: path

    path = "1/"

    call system('mkdir -p ' // adjustl(trim(path)))

    call read_cont_table(cont,total)
    write(*,*) 'Total: ',total
    ins = 92094
    call read_constraints_gen(ins,c_gen)
    call read_constraints_sex(ins, c_sex)
    call read_constraints_dipl(ins, c_dipl)
    call read_constraints_stat(ins, c_stat)
    
    ct = ipf_gen(cont, c_gen, c_sex, c_dipl, c_stat, eps, ins, path)
    call trs(ct)

    call write_population(ct, ins, path)

   ! call read_age_distr(age_distr)
   ! pop = generate_pop(ct,age_distr)

   ! call read_birth_distr(birth_distr)
   ! call read_death_distr(death_distr)

   ! call write_population_list(pop,ins,2018)

   ! call simulate_year(pop,birth_distr,death_distr)

   ! call write_population_list(pop,ins,2019)

   !call write_cont_table(ct,ins)

end program main



