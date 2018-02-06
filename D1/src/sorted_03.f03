!Sorted arrays 03


subroutine is_sorted(inp, length, sorted)

  implicit none
  integer, intent(in) :: length
  integer, dimension(length), intent(in) :: inp
  logical, intent(out) :: sorted
  integer :: last, i

  sorted = .true.
  last = inp(1)

  do i=2, length
    if (last > inp(i)) then
      sorted = .false.
      exit
    end if

    last = inp(i)
  end do

end subroutine is_sorted


program sorted_03
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

  do i=1, length
   array_sum = array_sum + array(i) 
  end do

  call is_sorted(array, length, sorted)

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
end program sorted_03



