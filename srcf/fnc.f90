module fnc
 contains
 

  
  subroutine inflows_prep(inflows)
    use types
!     integer,  intent(in)      :: a
    type(inflowst), dimension(:,:), intent(in):: inflows
  
!    print *, a  
   print *, inflows(1,1)%n
  
  end subroutine inflows_prep
  
  
  subroutine make_ij(r,c,loop,bcmat)
    use types
    
    integer,  intent(in)      :: r,c
    type(kdepocitat), intent(inout):: loop
    integer, dimension(:,:), intent(in) :: bcmat
    
    integer :: i, j, n, in
    
    n = 0
    in = 1
    
    do i = 1, r
      do j = 1, c
        if (bcmat(i,j) == 0) then
          n = n + 1
        end if 
      end do
    end do
    
    loop%n = n
    allocate(loop%ij(n,2))
    
    do i = 1, r
      do j = 1, c
        if (bcmat(i,j) == 0) then
          loop%ij(in,:) = (/i,j/)
          in = in + 1
        end if 
      end do
    end do
    
    
    
    
  end subroutine make_ij
  
  
  
  
end module fnc