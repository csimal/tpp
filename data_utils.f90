module data_utils
    use constants
    use list

! """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
! File: data_utils.f90
! Author: meuricel, csimal
! Description: This module contains various functions and subroutines for
! reading/writing files for the simulation
! """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

    implicit none
    contains

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! functions for converting string values from files to corresponding ints !
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

! PRE: /
! POST: /
! Description: converts the string value of gen to the corresponding
! index in the contingency table. returns 0 if invalid
! Arguments:
! IN : gen is a string containing the age group
 function str2gen(gen) result(i)
    character(len=5), intent(in) :: gen
    integer :: i
    select case (gen)
        case ("0.4")
            i=1
        case ("5.9")
            i=2
        case ("10.14")
            i=3
        case ("15.19")
            i=4
        case ("20.24")
            i=5
        case ("25.29")
            i=6
        case ("30.34")
            i=7
        case ("35.39")
            i=8
        case ("40.44")
            i=9
        case ("45.49")
            i=10
        case ("50.54")
            i=11
        case ("55.59")
            i=12
        case ("60.64")
            i=13
        case ("65.69")
            i=14
        case ("70.74")
            i=15
        case ("75.79")
            i=16
        case ("80.84")
            i=17
        case ("85.89")
            i=18
        case ("90.94")
            i=19
        case ("95.")
            i=20
        case default
            i=0
    end select
end function str2gen

! PRE: /
! POST: /
! Description: converts the string value of sex to the corresponding index
! in the contingency table. returns 0 if invalid.
! Arguments:
! IN : sex is a string containing either "Hommes" or "Femmes"
function str2sex(sex) result(i)
    character(len=6), intent(in) :: sex
    integer :: i
    select case (sex)
            case ("Hommes")
                i=1
            case ("Femmes")
                i=2
            case default
                i=0
    end select
end function str2sex

! PRE: /
! POST: /
! Description: converts the string value of dipl to the corresponding index
! in the contingency table. returns 0 if invalid
! Arguments:
! IN : dipl is a string containing the education level
function str2dipl(dipl) result(i)
    character(len=11), intent(in) :: dipl
    integer :: i
    select case (dipl)
            case ("Aucun")
                i=1
            case ("CITE1")
                i=2
            case ("CITE2")
                i=3
            case ("CITE3")
                i=4
            case ("CITE4")
                i=5
            case ("CITE5")
                i=6
            case("CITE6")
                i=7
            case("NonConcerne")
                i=8
            case default
                i=0
    end select
end function str2dipl

! PRE: /
! POST: /
! Description: converts the string value of stat to the corresponding index
! in the contigency table. returns 0 if invalid
! Arguments:
! IN : statut is a string containing the work status ("Chomeurs", "Inactifs" or "Travailleurs")
function str2stat(statut) result(i)
    character(len=12), intent(in) :: statut
    integer :: i
    select case (statut)
        case ("Chomeurs")
            i=1
        case ("Inactifs")
            i=2
        case ("Travailleurs")
            i=3
    end select
end function str2stat

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! functions for converting index values computed by the above functions to their !
! string counterparts. They can be thought of as their inverse                   !
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

! PRE: /
! POST: /
! Description: converts the generation index i to the corresponding string.
! returns an empty string if invalid
! Arguments:
! IN : i is an integer between 1 and 20
function gen2str(i) result(gen)
    integer, intent(in) :: i
    character(len=5) :: gen
    select case (i)
        case (1)
            gen= "0.4"
        case (2)
            gen="5.9"
        case (3)
            gen="10.14"
        case (4)
            gen="15.19"
        case (5)
            gen="20.24"
        case (6)
            gen="25.29"
        case (7)
            gen="30.34"
        case (8)
            gen="35.39"
        case (9)
            gen="40.44"
        case (10)
            gen="45.49"
        case (11)
            gen="50.54"
        case (12)
            gen="55.59"
        case (13)
            gen="60.64"
        case (14)
            gen="65.69"
        case (15)
            gen="70.74"
        case (16)
            gen="75.79"
        case (17)
            gen="80.84"
        case (18)
            gen="85.89"
        case (19)
            gen="90.94"
        case (20)
            gen="95."
        case default
            gen = ""
    end select
end function gen2str

! PRE: /
! POST: /
! Description: converts an index for the sex variable to the corresponding
! string. returns an empty string if invalid
! Arguments:
! IN : i is an integer between 1 and 2
function sex2str(i) result(sex)
    character(len=6) :: sex
    integer, intent(in) :: i
    select case (i)
            case (1)
                sex="Hommes"
            case (2)
                sex="Femmes"
            case default
                sex=""
    end select
end function sex2str

! PRE: /
! POST: /
! Description: converts an index for the dipl variable to the corresponding
! string. returns an empty string if invalid
! Arguments:
! IN : i is an integer between 1 and 8
function dipl2str(i) result(dipl)
    character(len=11) :: dipl
    integer, intent(in) :: i
    select case (i)
            case (1)
                dipl="Aucun"
            case (2)
                dipl="CITE1"
            case (3)
                dipl="CITE2"
            case (4)
                dipl="CITE3"
            case (5)
                dipl="CITE4"
            case (6)
                dipl="CITE5"
            case(7)
                dipl="CITE6"
            case(8)
                dipl="NonConcerne"
            case default
                dipl=""
    end select
end function dipl2str

! PRE: /
! POST: /
! Description: converts an index for the stat variable to the corresponding
! string. returns an empty string if invalid
! Arguments:
! IN : i is and integer between 1 and 3
function stat2str(i) result(stat)
    character(len=12) :: stat
    integer, intent(in) :: i
    select case (i)
        case (1)
            stat="Chomeurs"
        case (2)
            stat="Inactifs"
        case (3)
            stat="Travailleurs"
    end select
end function stat2str

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! subroutines for reading files into useful data !
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

! PRE: BelgiqueConting.txt must be in the same directory as the calling
! executable
! POST: cont is the contingency table described in BelgiqueConting.txt
!       total contains the total population count
! Description: this subroutine reads a file containing a list of values
! preceded by 4 strings describing their index in the contingency table.
! lines are assumed to be of the form : "gen sex dipl stat val", and it is
! assumed that every possible choice of index appears at most once.
! (missing entries are assumed to be 0)
! Arguments:
! OUT : cont
! OUT : total is the total number of people in the population
subroutine read_cont_table(cont, total)
    real(dp), dimension(20,2,8,3),intent(out) :: cont
    integer, intent(out) :: total
    character(len=12) :: gen, sex, dipl, statut
    integer :: freq,iostatus,i,j,k,l

    cont = 0.0
    total = 0
    open (unit = 1, file = 'BelgiqueConting.txt', action='read')
    read(1,*) !skip first line
     do while (1.eq.1)
        read(1,*,IOSTAT=iostatus) gen, sex, dipl, statut, freq
        if (iostatus<0) then
            exit
        end if
        total = total+freq
        i = str2gen(gen)
        j = str2sex(sex)
        k = str2dipl(dipl)
        l = str2stat(statut)
        cont(i,j,k,l) = freq
     end do
     close(1)
end subroutine read_cont_table

! PRE: if provided, path must contain the path to an existing directory in the form "dir/"
! POST: cont is written to a file
! Description: writes the contingency table cont to a file according to
! the format used in read_cont_table. The file created is named
! "cont_table_XXXXX.txt", where XXXXX is the 5 digit ins code
! If a file with this name already exists, it is overwritten.
! If the optional path argument is provided, the file will be created in that path
! provided the path points to an existing directory.
! Arguments:
! IN : cont is the contingency table to be written
! IN : ins is the ins code for the commune
! IN : path is the optional path where the file should be created
subroutine write_cont_table(cont, ins, path)
    real(dp), dimension(20,2,8,3),intent(in) :: cont
    integer, intent(in) :: ins
    character(len=4), intent(in), optional :: path
    integer :: i, j, k, l
    character(len=35) :: filename
    character(len=30) :: formatstr

    write(filename,'(A11,I5,A4)') 'cont_table_', ins, '.txt'
    if (present(path)) then
        filename = trim(adjustl(path))//adjustl(filename)
    end if
    open (unit = 10, file = filename, action='write')
    formatstr = '(A,A,A,A,A,A,A,A,I6)'
    write(10,'(A)') 'gener sex dipl statut Freq'
    do l = 1,3
        do k = 1,8
            do j = 1,2
                do i = 1,20
                    write(10,formatstr) trim(gen2str(i)),' ',sex2str(j),' ', & 
                        trim(dipl2str(k)),' ',trim(stat2str(l)),' ',int(cont(i,j,k,l))
                end do
            end do
        end do
    end do
    close(10)
end subroutine write_cont_table

! PRE: if provided, path must contain the path to an existing directory in the form "dir/"
! POST: writes the list of individuals from cont into a file
! Description: writes the population described in cont to a file.
! Each line describes an individual from cont using the following format:
! <generation> <sex> <diploma> <work_status>
! The file created is named "population_generated_XXXXX.txt"
! where XXXXX is the 5 digit ins code. If a file with this name
! already exists, it is overwritten.
! If the optional path argument is provided, the file will be created in that path
! provided the path points to an existing directory.
! Arguments:
! IN : cont is the contingency table
! IN : ins is the ins code for the commune
! IN : path is the optional path where the file should be created
subroutine write_population(cont, ins, path)
    real(dp), dimension(20,2,8,3),intent(in) :: cont
    integer, intent(in) :: ins
    character(len=4), intent(in), optional :: path
    integer :: i, j, k, l,m
    character(len=40) :: filename

    write(filename,"(A,I5,A)") "final_pop_", ins, ".txt"
    if (present(path)) then
        filename = trim(adjustl(path))//adjustl(filename)
    end if
    open (unit = 10, file = filename, action="write")
    write(10,'(A)') 'gener sex dipl statut'
    do l = 1,3
        do k = 1,8
            do j = 1,2
                do i = 1,20
                    do m=1,int(cont(i,j,k,l))
                        write(10,'(I2,A,I1,A,I1,A,I1)') i,' ',j,' ', & 
                            k,' ',l
                    end do
                end do
            end do
        end do
    end do
    close(10)
end subroutine write_population

! PRE: if provided, path must contain the path to an existing directory in the form "dir/"
! POST: writes the list of individuals from pop into a file
! Description:
! Each line describes an individual from pop using the following format:
! <generation> <sex> <diploma> <work_status>
! The file created is named "population_XXXXX_YYYY.txt"
! where XXXXX is the 5 digit ins code, and YYYY is the year.
! If a file with this name already exists, it is overwritten.
! If the optional path argument is provided, the file will be created in that path
! provided the path points to an existing directory.
! Arguments:
! IN : pop is the list of individuals
! IN : ins
! IN : year
! IN : path is the optional path where the file should be created
subroutine write_population_list(pop, ins, year, path)
    type(ListPerson), intent(in) :: pop
    integer, intent(in) :: ins, year
    character(len=4), intent(in), optional :: path
    character(len=40) :: filename
    type(ListPersonNode), pointer :: node, empty
    type(Person), pointer :: p

    write(filename,"(A,I5,A,I4,A)") 'population_', ins,'_',year, '.txt'
    if (present(path)) then
        filename = trim(adjustl(path))//adjustl(filename)
    end if

    open (unit = 10, file = filename, action='write')
    write(10,'(A)') 'age sex dipl statut'
    allocate(empty)
    node => empty
    node%right => pop%first
    do while (associated(node%right))
        node => node%right
        p => node%person
        write(10,'(I3,A,I1,A,I1,A,I1)') p%age,' ',p%sex,' ', &
            p%diploma,' ',p%work_status
    end do
    deallocate(empty)
    close(10)
end subroutine write_population_list

! PRE: if provided, path must contain the path to an existing directory in the form "dir/"
! POST: writes the population described in pop as a contingency table
! Description: Each entry in the file is of the form 
! <gener> <sex> <dipl> <statut> <Freq>, that is the same format used in read_cont_table()
! Only entries with Freq > 0 are written, in order to keep filesize low.
! The file created is called "cont_table_final_YYYY_XXXXX.txt", where YYYY is year, and XXXXX is the 5-digit ins code
! If the optional path argument is provided, the file will be created in that path
! provided it points to an existing directory.
! Arguments:
! IN : pop is the list of individuals
! IN : ins
! IN : year
! IN : path is the optional path where the file should be created
subroutine write_final_cont(pop,ins,year,path)
    type(ListPerson), intent(in) :: pop
    integer, intent(in) :: ins,year
    integer :: age,i,j,k,l
    real(dp), dimension(20,2,8,3) :: cont
    character(len=4), intent(in), optional :: path
    character(len=40) :: filename
    type(ListPersonNode), pointer :: node, empty
    type(Person), pointer :: p
    character(len=30) :: formatstr

    cont = 0.0 ! this initializes every element of cont to 0.0
    
    allocate(empty)
    node => empty
    node%right => pop%first
    do while (associated(node%right))
        node => node%right
        p => node%person

        age = int(p%age/5)+1
        if( p%age >= 95) then
            age = 20
        end if
        cont(age,p%sex,p%diploma,p%work_status)=cont(age,p%sex,p%diploma,p%work_status)+1
    end do

    write(filename,'(A,I4,A,I5,A)') 'cont_table_final_',year,'_', ins, '.txt'
    if (present(path)) then
        filename = trim(adjustl(path))//adjustl(filename)
    end if
    open (unit = 10, file =filename, action='write')
    write(10,'(A)') 'gener sex dipl statut Freq'
    formatstr = '(A,A1,A,A1,A,A1,A,A1,I6)'
    do l = 1,3
        do k = 1,8
            do j = 1,2
                do i = 1,20
                    if (int(cont(i,j,k,l)) /= 0) then ! only print non zero elements for storage efficiency
                        write(10,formatstr) trim(gen2str(i)),' ',sex2str(j),' ', &
                            trim(dipl2str(k)),' ',trim(stat2str(l)),' ',int(cont(i,j,k,l))
                    end if
                end do
            end do
        end do
    end do
    close(10)
end subroutine write_final_cont

! PRE: "ContrainteAge.txt" must be in the same directory as the calling
! executable
! POST: const_gen contains the generation constraint table for ins
! Description: reads the constraint table associated with ins from
! a local file
! Arguments:
! IN : ins
! INOUT : const_gen is table containing the age group contraints for the population
subroutine read_constraints_gen(ins,const_gen)
    integer, dimension(20), intent(inout) :: const_gen
    integer, intent(in) :: ins
    integer :: iostatus
    character(len=5) :: gen
    integer :: com, val

    open (unit = 1, file = 'ContrainteAge.txt', action='read')
    read(1,*)
    do while (1.eq.1)
        read(1,*,IOSTAT=iostatus) com, gen, val
        if (iostatus<0) then
            exit
        end if
        if (com==ins) then
            const_gen(str2gen(gen)) = val
        end if
    end do
    close(1)
end subroutine read_constraints_gen

! PRE: "ContrainteGenre.txt" must be in the same directory
!       as the calling executable
! POST: const_sex contains the gender constraints for ins
! Description: reads the constraint table associated with ins from
! a local file
! Arguments:
! IN : ins
! INOUT : const_sex is table containing the gender contraints for the population
subroutine read_constraints_sex(ins,const_sex)
    integer, dimension(2), intent(inout) :: const_sex
    integer, intent(in) :: ins
    integer :: iostatus,i
    character(len=6) :: sex
    integer :: com, val

    open (unit = 1, file = 'ContrainteGenre.txt', action='read')
    read(1,*)
    do while (1.eq.1)
        read(1,*,IOSTAT=iostatus) com, sex, val
        if (iostatus<0) then
            exit
        end if
        if (com==ins) then
            i=str2sex(sex)
            const_sex(i)=val
        end if
    end do
    close(1)
end subroutine read_constraints_sex

! PRE: "ContrainteDipl.txt" must be in the same directory
!       as the calling executable
! POST: const_dipl contains the diploma constraints for ins
! Description: reads the constraint table associated with ins from
! a local file
! Arguments:
! IN : ins
! INOUT : const_dipl is table containing the diploma contraints for the population
subroutine read_constraints_dipl(ins,const_dipl)
    integer, dimension(8), intent(inout) :: const_dipl
    integer, intent(in) :: ins
    integer :: iostatus,i
    character(len=11) :: dipl
    integer :: com, val

    open (unit = 1, file = 'ContrainteDipl.txt', action='read')
    read(1,*)
    do while (.true.)
        read(1,*,IOSTAT=iostatus) com, dipl, val
        if (iostatus<0) then
            exit
        end if
        if (com==ins) then
            i=str2dipl(dipl)
            const_dipl(i)=val
        end if
    end do
    close(1)
end subroutine read_constraints_dipl

! PRE: "ContrainteStatut.txt" must be in the same directory
!       as the calling executable
! POST: const_stat contains the work status constraints for ins
! Description: reads the constraint table associated with ins from
! a local file
! Arguments:
! IN : ins
! INOUT : const_stat is table containing the work status contraints for the population
subroutine read_constraints_stat(ins,const_stat)
    integer, dimension(3), intent(inout) :: const_stat
    integer, intent(in) :: ins
    integer :: iostatus,i
    character(len=12) :: stat
    integer :: com, val

    open (unit = 1, file = 'ContrainteStatut.txt', action='read')
    read(1,*)
    do while (.true.)
        read(1,*,IOSTAT=iostatus) com, stat, val
        if (iostatus<0) then
            exit
        end if
        if (com==ins) then
            i = str2stat(stat)
            const_stat(i) = val
        end if
    end do
    close(1)
end subroutine read_constraints_stat

! PRE: "ContrainteGenre.txt" must be in the same directory
!       as the calling executable
! POST: ins_table contains the list of ins codes that can be processed
! Description: reads the list of ins codes from a local file
! Arguments:
! IN : ins
! OUT : ins_table is table containing the ins codes to be used in the simulation
subroutine read_ins_table(ins_table)
    integer, dimension(38), intent(out) :: ins_table
    integer :: i

    open (unit = 1, file = 'ContrainteGenre.txt', action='read')
    read(1,*)
    do i=1,38
        read(1,*) ins_table(i)
    end do
    close(1)
end subroutine read_ins_table

end module data_utils
