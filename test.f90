program main
    use random
    use constants
    integer :: n = 1000000
    integer :: i,j
    real(dp), dimension(100) :: freq_table
    integer :: max_val = 2**31 -1!2**62 -1
    real(dp) :: r, m, chi
    
    do i = 1,100 
        freq_table(i) = 0
    end do
    open(unit=10,file="rtable.txt",action="write")
    do i = 1,n 
        r = float(randu())/float(max_val)
        write(10,*) r
        j=0
        do while (r>=j*0.01)
            j=j+1
        end do
        freq_table(j) = freq_table(j) + 1
    end do
    close(10)
    chi = 0.0
    m = n*0.01
    do i = 1,100 
        chi = chi + ((freq_table(i)-m)**2)/m
    end do
    write(*,*) max_val
    write(*,*) chi

    open (unit = 10, file = "ftable.txt", action="write") 
    do i = 1,100
        write(10,*) freq_table(i)        
    end do
    close(10)

end program main
