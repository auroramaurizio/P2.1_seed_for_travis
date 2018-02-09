!16 Find next prime

program next_prime

    implicit none

    interface
        function prime(num) result (res)
            integer, intent(in) :: num
            integer:: res
        end function prime
    end interface

    integer::num
   
    read (*,*) num
    print *, "Given number:", num
    print *, "Next prime:", prime(num)


end program next_prime


function prime(num) result (res)

    integer , intent(in) :: num
    integer :: i, j
    integer :: res
    res=0

    do i=num+1, 2*num-1
        do j = 2, ( 2*num )-2
                if ( mod(i,j)==0 ) exit                
        enddo
        if ( j .eq. i ) then
                res=i
                exit
        endif
    enddo

end function prime

