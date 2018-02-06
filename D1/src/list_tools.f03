MODULE list_tools

  implicit none

contains 

SUBROUTINE is_sorted(inp, length, sorted)

  implicit none
  integer, intent(in) :: length
  real, dimension(length), intent(in) :: inp
  logical, intent(out) :: sorted
  real :: last
  integer :: i

  sorted = .true.
  last = inp(1)

  do i=2, length
    if (last > inp(i)) then
      sorted = .false.
      exit
    end if

    last = inp(i)
  end do

END SUBROUTINE is_sorted

END MODULE list_tools
