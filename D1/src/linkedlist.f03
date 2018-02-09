module linked_list
  use list_types
  implicit none

interface insert
   module procedure insert
end interface insert

interface delete
   module procedure delete
end interface delete

interface lookup
   module procedure lookup
end interface lookup


  type listElem
    type(pair) :: kv
    type(listElem), pointer :: next => null()
  end type listElem


contains

  subroutine insert(head, p)
    type(listElem), pointer, intent(inout) :: head
    type(pair), intent(in)                 :: p
    type(listElem), pointer                :: elem
    allocate(elem)
    elem%kv = p
    elem%next => head%next
    head%next => elem
  end subroutine insert


  subroutine delete(head)
    type(listElem), pointer, intent(inout) :: head
    type(listElem), pointer             :: ptr
    do while( associated(head%next) )
      ptr => head%next
      head%next => head%next%next
      deallocate(ptr)
    end do
  end subroutine delete


  subroutine lookup(head, idx, p)
    type(listElem), pointer, intent(in) :: head
    integer, intent(in)                 :: idx
    type(listElem), pointer             :: ptr
    type(pair), intent(out)             :: p
    p%key = idx-1
    ptr => head
    do while( associated(ptr%next) )
      if( ptr%next%kv%key .eq. idx ) then
        p = ptr%next%kv
      endif
        ptr => ptr%next
    end do
  end subroutine lookup



end module linked_list
