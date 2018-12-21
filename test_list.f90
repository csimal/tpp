program test_list
    use list

    implicit none
    type(Person), pointer :: p
    type(ListPerson) :: l1,l2
    type(ListPersonNode), pointer :: node, empty
    integer(8) :: i
    character(len=19) :: str = '(I2,X,I1,X,I1,X,I1)'

    write(*,*) 'Initializing lists'

    do i = 1, 10
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
        write(*,str) node%person%age, node%person%sex, node%person%diploma, node%person%work_status
    end do
    
    write(*,*) 'Deleting first element'
    call remove_person(l1,l1%first)
    node => empty
    node%right => l1%first

    do while ( associated(node%right) )
        node => node%right
        write(*,str) node%person%age, node%person%sex, node%person%diploma, node%person%work_status
    end do

    write(*,*) 'Deleting last element'
    call remove_person(l1,l1%last)
    node => empty
    node%right => l1%first

    do while ( associated(node%right) )
        node => node%right
        write(*,str) node%person%age, node%person%sex, node%person%diploma, node%person%work_status
    end do

    write(*,*) 'Deleting middle element'
    node => l1%first
    node => node%right%right ! deleting third element
    call remove_person(l1,node)
    node => empty
    node%right => l1%first

    do while ( associated(node%right) )
        node => node%right
        write(*,str) node%person%age, node%person%sex, node%person%diploma, node%person%work_status
    end do

    deallocate(empty)
    call empty_list(l1)

end program test_list
