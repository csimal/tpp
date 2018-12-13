program main
    use data_utils
    use constants
    use ipf
    use age
    use list
    use pop_sim

    implicit none
    integer :: total,ins,i,j,year,n_men,n_women,seed
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
    integer, dimension(38) :: ins_table
    character(len=30) :: filename,arg
    character(len=4) ::path

    call getarg(1,arg)
    read(arg,*) year

    call getarg(2,path)
    read(path,*) seed

    write(path,"(A3,A1)") trim(path),"/"
    call system('mkdir -p ' // adjustl(path))
    write(*,*) seed
    seed = random_xorshift(seed)

    call read_cont_table(cont,total)
    call read_ins_table(ins_table)
    call read_age_distr(age_distr)
    call read_birth_distr(birth_distr)
    call read_death_distr(death_distr)
    do i=1,38
        ins=ins_table(i)
        write(*,*) "Simulation for ins:",ins
        write(filename,"(A9,I5,A4)") "sextable_", ins,".txt"
        filename = trim(adjustl(path))//adjustl(filename)
        open (unit = 2,file=filename, action="write")
        write(2,*)"Années"," ","Hommes"," ","Femmes"
        call read_constraints_gen(ins,c_gen)
        call read_constraints_sex(ins, c_sex)
        call read_constraints_dipl(ins, c_dipl)
        call read_constraints_stat(ins, c_stat)

        ct = ipf_gen(cont, c_gen, c_sex, c_dipl, c_stat, eps, ins, path)
        call trs(ct)
        call write_population(ct, ins, path)
        n_men=int(sum(ct(:,1,:,:)))
        n_women=int(sum(ct(:,2,:,:)))
        write(2,*)2011,n_men,n_women
        pop = generate_pop(ct,age_distr)
        do j=2011,year-1
                call simulate_year(pop,birth_distr,death_distr,n_men,n_women)
                write(2,*)j+1,n_men,n_women
        end do
        write(*,*) "finished simulating"
        call write_population_list(pop,ins,year,path)
        call write_final_pop(pop,ins,year,path)
        write(*,*) "Cleaning up"
        call empty_list(pop)
        close(2)
    end do
end program main
