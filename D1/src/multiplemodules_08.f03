!08 Multiple modules

program multiplemodules_08
  use list_tools

  implicit none
  integer :: i, c, length
  logical :: sorted_key, sorted_value
  real :: rounded, check_sum, array_sum = 0
  type (pair), allocatable, dimension(:) :: array
  c = 5
  read(c, *) length

  allocate(array(length))

  read(c, *) array
  read(c, *) check_sum

 
  call is_sorted(array, length, sorted_key, descending, bykey)
  call is_sorted(array, length, sorted_value, ascending, byvalue)

  print *, "Array length= ", length
  
  if (sorted_key) then
    print *, "The array is sorted in descending order by key."
  else
    print *, "The array is not sorted in descending order by key."
  end if

  if (sorted_value) then
    print *, "The array is sorted in ascending order by value."
  else
    print *, "The array is not sorted in ascending order by value."
  end if

  do i=1, length
   array_sum = array_sum + array(i)%key
  end do

  rounded = abs( 1-check_sum/array_sum )

  if (rounded < 1e-5) then
    print *, "Match check."
  else

    print *, "No match in sum."
    print *, "Expected sum:", check_sum
    print *, "Calculated sum:", array_sum
  end if

  deallocate(array)
end program multiplemodules_08



