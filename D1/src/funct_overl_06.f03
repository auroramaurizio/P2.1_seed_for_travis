!06 Function overloading

program funct_overl_06
  use list_tools
  implicit none
  integer :: i, c
  logical :: sorted
  integer :: length, check_sum, array_sum = 0
  integer, allocatable, dimension(:) :: array
  c = 5
  read(c, *) length
  allocate( array(length) )

  read(c, *) array
  read(c, *) check_sum
  call is_sorted(array, length, sorted)
  do i=1, length
   array_sum = array_sum + array(i) 
  end do

  print *, "Array length=  ", length

  if (sorted) then
    print *, "The array is sorted."
  else
    print *, "The array is not sorted."
  end if

  if (check_sum == array_sum) then
    print *, "Match check."
  else

    print *, "No match in sum."
    print *, "Expected sum:", check_sum
    print *, "Calculated sum:", array_sum
  end if

  deallocate( array )
end program funct_overl_06



