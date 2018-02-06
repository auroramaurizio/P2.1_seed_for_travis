!Real arrays 02

program readreal_02
  implicit none
  integer :: i, c
  integer :: length
  real :: check_sum, rounded, array_sum = 0
  real, allocatable, dimension(:) :: array
  c=5
  read(c, *) length
  allocate( array(length) )

  read(c, *) array
  read(c, *) check_sum

  do i=1, length
   array_sum = array_sum + array(i) 
  end do

  rounded = abs( 1 - check_sum/array_sum )

  print *, "Array length= ", length

  if (rounded < 1e-5) then
    print *, "Match check."
  else
    print *, "No match in sum."
    print *, "Expected sum:", check_sum
    print *, "Calculated sum:", array_sum
  end if

  deallocate( array )
end program readreal_02
