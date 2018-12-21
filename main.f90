program main
    use data_utils
    use constants
    use ipf
    use age
    use list
    use pop_sim

    implicit none
    integer :: total,ins,i,j,k,year,seed
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
    integer,dimension(2,105) :: age_year
    
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
    do i=23,23
        ins=ins_table(i)
        age_year =0
        ct = 0
        write(*,*) "Simulation for ins:",ins
        write(filename,"(A9,I5,A4)") "sextable_", ins,".txt"
        filename = trim(adjustl(path))//adjustl(filename)
        open (unit = 2,file=filename, action="write")
        write(2,'(A24)')"Année Age Hommes Femmes"
        call read_constraints_gen(ins,c_gen)
        call read_constraints_sex(ins, c_sex)
        call read_constraints_dipl(ins, c_dipl)
        call read_constraints_stat(ins, c_stat)

        ct = ipf_gen(cont, c_gen, c_sex, c_dipl, c_stat, eps, ins, path)
        call trs(ct)
        call write_population(ct, ins, path)
        call generate_pop(ct,age_distr,pop,age_year)
        do k=1,105
            write(2,"(I4,A1,I3,A1,I7,A1,I7)")2011,' ', k-1,' ',age_year(1,k),' ',age_year(2,k)
        end do
        do j=2011,year-1
                call simulate_year(pop,birth_distr,death_distr,age_year)
                do k=1,105
                    write(2,"(I4,A1,I3,A1,I7,A1,I7)")j+1,' ', k-1,' ',age_year(1,k),' ',age_year(2,k)
                end do
        end do
        call write_population_list(pop,ins,year,path)
        call write_final_pop(pop,ins,year,path)
        call empty_list(pop)
        close(2)
    end do
end program main
