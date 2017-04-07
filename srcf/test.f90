subroutine prt(A)
  use test2

  implicit none
  real, intent(in), dimension(:,:) :: A
  
  integer :: i, n
  
  n = ubound(A,1)
  
  do i = 1, n
    print *, A(i,:)
  end do
  call prt2()
end subroutine prt