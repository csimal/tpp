subroutine write_population(cont, ins)
    real(dp), dimension(20,2,8,3),intent(in) :: cont
    integer, intent(in) :: ins
    integer :: i, j, k, l,m
    character(len=30) :: filename

    write(filename,"(A21,I5,A4)") "population_generated_", ins, ".txt"
    open (unit = 10, file = filename, action="write")     
    do l = 1,3 
        do k = 1,8
            do j = 1,2
                do i = 1,20
                    do m=1,int(cont(i,j,k,l))
                        write(10,*) gen2str(i),' ',sex2str(j),' ',dipl2str(k),' ',stat2str(l)
                    end do
                end do
            end do
        end do
    end do
    close(10)
end subroutine write_population
