MODULE list_tools

  implicit none

  logical :: ascending = .true.
  logical :: descending = .false.


  interface is_sorted
    module procedure is_sorted_int, is_sorted_real
  end interface is_sorted


contains 

SUBROUTINE is_sorted_int(inp, length, sorted, order)

  implicit none
  integer, intent(in) :: length
  integer, dimension(length), intent(in) :: inp
  logical, intent(out) :: sorted
  logical, optional :: order
  logical :: sort_order
  integer :: last
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
  else 
    do i=2, length
    if  (last < inp(i)) then
      sorted = .false.
      exit
    endif
    end do
  endif

              
last = inp(i)            
           

END SUBROUTINE is_sorted_int


SUBROUTINE is_sorted_real (inp, length, sorted, order)

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
  else 
    do i=2, length
    if  (last < inp(i)) then
      sorted = .false.
      exit
    endif
    end do
  endif

              
last = inp(i)            
           

END SUBROUTINE is_sorted_real

END MODULE list_tools
