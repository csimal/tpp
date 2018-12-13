program test_list
    use list

    implicit none
    type(Person), pointer :: p
    type(ListPerson) :: l1,l2
    type(ListPersonNode), pointer :: node, empty
    integer :: i

    write(*,*) 'Initializing lists'

    do i = 1, 15
        call new_person(i,0,0,0,p)
        call append_person(l1,p)
        call new_person(i,1,0,0,p)
        call append_person(l2,p)
    end do

    write(*,*) 'Concatenating'

    call concat_lists(l1,l2)

    write(*,*) 'Traversing'

    allocate(empty)
    node => empty
    node%right => l1%first

    do while ( associated(node%right) )
        node => node%right
        write(*,*) node%person%age, node%person%sex, node%person%diploma, node%person%work_status
    end do
    
    deallocate(empty)
    call empty_list(l1)

end program test_list
