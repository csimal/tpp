program main
    use data_utils
    use constants
    use ipf
    use age
    use list
    use pop_sim

! """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
! File: main.f90
! Author: meuricel
! Description: This is the main program for the simulation.
! It takes as arguments the year the simulation should end at
! and an integer between 1 and 999 as the seed for the random number
! generator.
! The programs starts by initializing various objects needed for the
! simulation, and then performs the simulation for each ins code in
! order. See readme.txt for information on output files
! """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

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
    character(len=4) :: path
    integer,dimension(2,105) :: sextable
    
    call getarg(1,arg)
    read(arg,*) year

    call getarg(2,path)
    read(path,*) seed

    write(path,'(A3,A1)') trim(path),'/'
    call system('mkdir -p ' // adjustl(path)) ! create the directory
    write(*,'(A,I3)') 'Seed: ',seed
    seed = random_xorshift(seed) ! initialize the pseudorandom number generator with the seed

    call read_cont_table(cont,total)
    call read_ins_table(ins_table) ! read the list of ins codes to process
    call read_age_distr(age_distr)
    call read_birth_distr(birth_distr)
    call read_death_distr(death_distr)

    do i = 1,38
        ins = ins_table(i)
        sextable = 0
        ct = 0
        write(*,'(A,I5)') 'Simulation for ins: ',ins
        write(filename,'(A,I5,A)') 'sextable_', ins,'.txt'
        filename = trim(adjustl(path))//adjustl(filename)

        call read_constraints_gen(ins,c_gen)
        call read_constraints_sex(ins, c_sex)
        call read_constraints_dipl(ins, c_dipl)
        call read_constraints_stat(ins, c_stat)

        ct = ipf_gen(cont, c_gen, c_sex, c_dipl, c_stat, eps, ins, path)
        call trs(ct)
        call write_population(ct, ins, path)
        call generate_pop(ct,age_distr,pop,sextable)

        open (unit = 2,file=filename, action='write')
        write(2,'(A)') 'Année Age Hommes Femmes'
        do k=1,105
            write(2,'(I4,A,I3,A,I7,A,I7)') 2011,' ', k-1,' ',sextable(1,k),' ',sextable(2,k)
        end do
        do j=2012,year
                call simulate_year(pop,birth_distr,death_distr,sextable)
                do k=1,105
                    write(2,'(I4,A,I3,A,I7,A,I7)') j,' ', k-1,' ',sextable(1,k),' ',sextable(2,k)
                end do
        end do

        call write_population_list(pop,ins,year,path)
        call write_final_cont(pop,ins,year,path)
        call empty_list(pop) ! clean up before the next simulation
        close(2)
    end do
end program main
