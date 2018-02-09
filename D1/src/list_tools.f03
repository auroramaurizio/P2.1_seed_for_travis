MODULE list_tools

  use list_types

  implicit none

  logical :: ascending = .true.
  logical :: descending = .false.
  logical :: bykey = .true.
  logical :: byvalue  = .false.


  interface is_sorted
    module procedure is_sorted_int, is_sorted_real, is_sorted_pair
  end interface is_sorted


contains 


! pick two randomly chosen elements in array 'dat'
! and swap them. do this 'count' times.
SUBROUTINE swap(dat,count)
  IMPLICIT NONE
  REAL, DIMENSION(:),INTENT(inout) :: dat
  INTEGER, INTENT(in) :: count
  REAL,DIMENSION(2) :: rval
  INTEGER :: i,num,i1,i2
  REAL :: tmp

  num = SIZE(dat,1)
  DO i=1,count
      ! pick two elements at random
      CALL RANDOM_NUMBER(rval)
      rval = rval*REAL(num)+0.5
      i1 = INT(rval(1))
      i2 = INT(rval(2))
      ! paranoia check to avoid out-of-bounds access
      IF ((i1 < 1) .OR. (i1 > num) .OR. (i2 < 1) .OR. (i2 > num)) CYCLE
      ! swap the elements
      tmp = dat(i1)
      dat(i1) = dat(i2)
      dat(i2) = tmp
  END DO
END SUBROUTINE swap


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


SUBROUTINE is_sorted_pair (inp, length, sorted, order, check)

    implicit none
    integer, intent(in) :: length
    type(pair), dimension(length), intent(in) :: inp
    logical, intent(out) :: sorted
    logical, optional :: order, check
    logical :: sort_order

    sort_order = ascending 
    if( present(order) ) sort_order = order

    sort_order = bykey 
    if( present(check) ) sort_order = check


    if (check .eqv. bykey) then
       call is_sorted_int(inp%key, length, sorted, order)
    else if (check .eqv. byvalue) then
      call is_sorted_real(inp%val, length, sorted, order)
    endif


END SUBROUTINE is_sorted_pair


END MODULE list_tools
