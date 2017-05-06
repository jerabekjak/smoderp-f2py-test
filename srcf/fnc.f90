module fnc
 contains
 

  
  subroutine make_inflows(r,c,inflows,fd)
    use types
    integer,  intent(in)      :: r, c
    type(inflowst), dimension(:,:), intent(inout):: inflows
    integer, dimension(:,:), intent(in):: fd
    integer, dimension(8,2)  :: wrkin
    integer, dimension(3,3)  :: okno
    integer :: i, j, ii, jj, n
    
    do i = 2, (r-1)
      do j = 2, (c-1)
        
        n = 0
        
        okno = fd((i-1):(i+1),(j-1):(j+1))
        
        if (okno(2,1) == 1) then
          n = n + 1
          wrkin(n,:) = (/i,j-1/)
        end if
        if (okno(1,1) == 2) then
          n = n + 1
          wrkin(n,:) = (/i-1,j-1/)
        end if
        if (okno(1,2) == 4) then
          n = n + 1
          wrkin(n,:) = (/i-1,j/)
        end if
        if (okno(1,3) == 8) then
          n = n + 1
          wrkin(n,:) = (/i-1,j+1/)
        end if
        if (okno(2,3) == 16) then
          n = n + 1
          wrkin(n,:) = (/i,j+1/)
        end if
        if (okno(3,3) == 32) then
          n = n + 1
          wrkin(n,:) = (/i+1,j+1/)
        end if
        if (okno(3,2) == 64) then
          n = n + 1
          wrkin(n,:) = (/i+1,j/)
        end if
        if (okno(3,1) == 128) then
          n = n + 1
          wrkin(n,:) = (/i+1,j-1/)
        end if
      
        if (n>0) then
          inflows(i,j)%n = n
          allocate(inflows(i,j)%in(1:n,2))
          inflows(i,j)%in(1:n,:) = wrkin(1:n,:)
        end if 
        
      
 
        
        
      end do
    end do
    


  
  end subroutine make_inflows
  
  
  
  
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
  
  
  !> prevede pripravenou combinatIndex na fortran
  !  type 1d pole zacinajici od 0 (python)
  subroutine make_infiltration(combinatIndex,infcoef)
    use types
    real, dimension(:,:), intent(in) :: combinatIndex
    type(infcoeft), dimension(0:), intent(inout) :: infcoef
    
    
    integer :: i, n
    
    n = ubound(combinatIndex,1)
    
    do i = 1, n
      infcoef(combinatIndex(i,1))%k = combinatIndex(i,2)
      infcoef(combinatIndex(i,1))%s = combinatIndex(i,3)
    end do
    
    
    
  
  end subroutine make_infiltration
  
  
  
  
  
  
  
  
  
  
end module fnc