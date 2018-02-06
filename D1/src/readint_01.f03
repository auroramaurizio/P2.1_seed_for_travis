program readint
  implicit none
  integer :: i, c 
  integer :: length, check_sum, array_sum = 0
  integer, dimension(:), allocatable :: array
  c=5
  read(c, *) length
  allocate( array(length) )

  read(c, *) array
  read(c, *) check_sum

  do i=1, length
   array_sum = array_sum + array(i) 
  end do

  print *, "Array length= ", length

  if (check_sum == array_sum) then
    print *, "Match check"
  else
    print *, "No match in sum."
    print *, "Expected sum:", check_sum
    print *, "Calculated sum:", array_sum
  end if

  deallocate( array )
end program readint

