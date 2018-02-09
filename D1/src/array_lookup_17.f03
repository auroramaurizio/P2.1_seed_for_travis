PROGRAM array_lookup
  USE list_types
  USE linked_list
  IMPLICIT NONE

  INTEGER :: num, i, j
  REAL :: chk, rv, time1, time2
  INTEGER, ALLOCATABLE, DIMENSION(:) :: idx
  TYPE(pair), ALLOCATABLE, DIMENSION(:) :: dat
  TYPE(pair) :: p
  type(listElem), pointer :: head => null()
  INTEGER, PARAMETER :: nlook = 50000

  ! Read from stdin length of data, data as array of pairs
  ! and finally checksum
  READ(5,*) num
  ALLOCATE(dat(num))
  READ(5,*) (dat(i),i=1,num)
  READ(5,*) chk

  ! fill linked list with items from dat() here
  allocate(head)
  do i=1, num
    call insert(head, dat(i))
  end do

  ! fill idx array with randomly selected keys
  CALL RANDOM_SEED()
  ALLOCATE(idx(nlook))
  DO i=1,nlook
      CALL RANDOM_NUMBER(rv)
      j = INT(rv*num)+1
      idx(i) = dat(j)%key
  END DO

  CALL CPU_TIME(time1)
  DO i=1,nlook
      DO j=1,num
          IF (dat(j)%key == idx(i)) THEN
              p = dat(j)
              EXIT
          END IF
      END DO
  END DO
  CALL CPU_TIME(time2)
  WRITE(*,FMT=666) nlook, 'array value lookups', (time2-time1)*1000.0

  CALL CPU_TIME(time1)
  DO i=1,nlook
    ! do linked list lookups here
    call lookup(head, idx(i), p) !AA
  END DO
  CALL CPU_TIME(time2)
  WRITE(*,FMT=666) nlook, 'll value lookups', (time2-time1)*1000.0

  ! free all allocated data
  DEALLOCATE(dat,idx)
  call delete(head) !AA
  deallocate(head)

666 FORMAT (' Performing',I8,1X,A20,1X,'took:',F12.6,' ms')     
END PROGRAM array_lookup
