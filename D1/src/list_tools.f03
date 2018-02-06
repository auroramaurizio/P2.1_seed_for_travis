MODULE list_tools

  implicit none

  logical :: ascending = .true.
  logical :: descending = .false.

contains 

SUBROUTINE is_sorted(inp, length, sorted, order)

  implicit none
  integer, intent(in) :: length
  real, dimension(length), intent(in) :: inp
  logical, intent(out) :: sorted
  logical, optional :: order
  logical :: sort_order
  real :: last
  integer :: i

  sort_order = ascending 
  if( present(order) ) sort_order = order

  sorted = .true.
  last = inp(1)

  if (sort_order .eqv. ascending ) then
    do i=2, length
    if  (last > inp(i)) then
      sorted = .false.
      exit
    endif
    end do
  else ! order .eqv. descending
    do i=2, length
    if  (last < inp(i)) then
      sorted = .false.
      exit
    endif
    end do
  endif

              
last = inp(i)            
           

END SUBROUTINE is_sorted

END MODULE list_tools
