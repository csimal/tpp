subroutine write_final_pop(pop, ins,year,path)
    type(ListPerson), intent(in) :: pop
    integer, intent(in) :: ins,year
    integer :: age,i,j,k,l
    real(dp), dimension(20,2,8,3) :: cont
    character(len=4), intent(in), optional :: path
    character(len=40) :: filename
    type(ListPersonNode), pointer :: node, empty
    type(Person), pointer :: p
    allocate(empty)
    node => empty
    node%right => pop%first
    do while (associated(node%right))
        node => node%right
        p => node%person
        
        age=int(p%age/5)+1
        if( p%age >= 95) then 
            age=20
        end if
        cont[age,p%sex,p%diplome,p%work_status]=cont[age,p%sex,p%diplome,p%work_status]+1
    end do
    
    write(filename,"(A17,I4,A1,I5,A4)") "cont_final_table_",year,"_", ins, ".txt"
    if (present(path)) then
        filename = trim(path) // filename
    end if
    open (unit = 10, file = filename, action="write")
    write(10,*) '"gener" "sex"    "dipl"   "statut"     "Freq"'
    do l = 1,3
        do k = 1,8
            do j = 1,2
                do i = 1,20
                    write(10,*) gen2str(i),' ',sex2str(j),' ',dipl2str(k),' ',stat2str(l),' ',int(cont(i,j,k,l))
                end do
            end do
        end do
    end do
    close(10)
end subroutine write_final_pop