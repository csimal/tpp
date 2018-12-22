module list
    implicit none

    type Person
        integer :: age
        integer :: sex
        integer :: diploma
        integer :: work_status
    end type Person

    type ListPersonNode
        type(Person), pointer :: person
        type(ListPersonNode), pointer :: left => null() ! previous element
        type(ListPersonNode), pointer :: right => null() ! next element
    end type ListPersonNode

    type ListPerson
        type(ListPersonNode), pointer :: first => null()
        type(listPersonNode), pointer :: last => null() ! for O(1) append
    end type ListPerson

    contains

! PRE: /
! POST: p is associated to a person defined by age, sex, dipl and wstat
subroutine new_person(age,sex,dipl,wstat,p)
    integer, intent(in) :: age
    integer, intent(in) :: sex
    integer, intent(in) :: dipl
    integer, intent(in) :: wstat
    type(Person), pointer, intent(out) :: p

    allocate(p)
    p%age = age
    p%sex = sex
    p%diploma = dipl
    p%work_status = wstat
end subroutine new_person

! PRE: node is contained in list
! POST: node is removed from list
subroutine remove_person(list,node)
    type(ListPerson), intent(inout) :: list
    type(ListPersonNode), pointer, intent(inout) :: node
    type(ListPersonNode), pointer :: temp, left, right

    temp => node ! keep the actual adress to free it without side effects

    if (.not. associated(node%left) .and. .not. associated(node%right) ) then
        nullify(list%first)
        nullify(list%last)
    elseif (.not. associated(node%left)) then
        right => node%right
        list%first => right
        nullify(list%first%left)
    elseif (.not. associated(node%right)) then
        left => node%left
        list%last => left
        nullify(left%right)
    else
        left => node%left
        right => node%right
        left%right => right
        right%left => left
    end if
    deallocate(temp%person)
    deallocate(temp)
end subroutine remove_person

! PRE: /
! POST: a node containing p is appended to list
subroutine append_person(list,p)
    type(ListPerson), intent(inout) :: list
    type(Person), pointer, intent(in) :: p

    if (.not. associated(list%first)) then ! empty list
        allocate(list%first)
        list%first%person => p
        list%last => list%first
    else
        allocate(list%last%right)
        list%last%right%person => p
        list%last%right%left => list%last
        list%last => list%last%right
    end if
end subroutine append_person

! PRE: /
! POST: l2 is appended to l1, and l2 is empty
subroutine concat_lists(l1,l2)
    type(ListPerson), intent(inout) :: l1,l2

    if ( associated(l1%first) .and. associated(l2%first) ) then
        l1%last%right => l2%first
        l2%first%left => l1%last
        l1%last => l2%last
        nullify(l2%first)
        nullify(l2%last)
    end if
end subroutine concat_lists

! PRE: /
! POST: deletes every element of list
subroutine empty_list(list)
    type(ListPerson), intent(inout) :: list

    do while ( associated(list%first) )
        call remove_person(list,list%first)
    end do
end subroutine empty_list

end module list
