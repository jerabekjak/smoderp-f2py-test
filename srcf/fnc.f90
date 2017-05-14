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
    implicit none
    integer,  intent(in)      :: r,c
    type(kdepocitat), intent(inout):: loop
    integer, dimension(:,:), intent(in) :: bcmat
    
    integer :: i, j, n, nbc, in, nin, ncin, bcin
    
    n   = 0  ! kolik bunek v oblasni
    nbc = 0  ! kolik bunek na okraji
    in  = 1  ! pozice v loop%ij
    nin = 1  ! pozice v loop%nc
    bcin= 1  ! pozice v loop%nbc
    
    do i = 1, r
      do j = 1, c
        if (bcmat(i,j) == 0) then
          n = n + 1
        end if 
        if (bcmat(i,j) == -99) then
          nbc = nbc + 1
        end if
      end do
    end do
    
    
    
    
    loop%n   = n
    loop%nbc = nbc
    loop%ntot = n + nbc
    
    allocate(loop%mi(  1:r,1:c   ))
    allocate(loop%ij(  n + nbc,2 ))
    allocate(loop%nc(  n ))
    allocate(loop%bcc( nbc ))
  

    
    
    do i = 1, r
      do j = 1, c
        if (bcmat(i,j) == 0) then
          loop%ij(in,:) = (/i,j/)
          loop%mi(i,j)  = in
          loop%nc(nin)  = in
          in = in + 1
          nin= nin + 1
        end if 
        if (bcmat(i,j) == -99) then
          loop%ij(in,:)   = (/i,j/)
          loop%mi(i,j)    = in
          loop%bcc(bcin)  = in
          in = in + 1
          bcin = bcin + 1
        end if 
      end do
    end do
    
    
    
!                               print *, 
!                               do i = 1, loop%n
!                                 print *, i, loop%nc(i)
!                               end do
!                               print *, 
!                               do i = 1, loop%nbc
!                                 print *, i, loop%bcc(i)
!                               end do
!                               print *, 
!                               do i = 1, loop%ntot
!                                 print *, i, loop%ij(i,:)
!                               end do
!                               print *, 
!                               do i = 1, ubound(loop%mi,1)
!                                 print *, loop%mi(i,:)
!                               end do
!                               print *, 
!                               do i = 1, ubound(bcmat,1)
!                                 print *, bcmat(i,:)
!                               end do
!                               stop
    
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
      infcoef(int(combinatIndex(i,1)))%k = combinatIndex(i,2)
      infcoef(int(combinatIndex(i,1)))%s = combinatIndex(i,3)
    end do
  
    
    
  
  end subroutine make_infiltration
  
  
  
  subroutine prtsparse(n,A)
    use types
    implicit none
    integer, intent(in)                        :: n
    type(sparsematt), dimension(:), intent(in) :: A
    
    real, dimension(n) :: wrk
    
    character(len=16)  :: form
    
    integer :: i, j, m, k
    
    
    
    write(form,'(a, i5,7a)') '(',n, 'e12.3)'
    
!     write(*,form) wrk
    
    
    do i = 1, n
      wrk = 0.0
      
      do k = 1, A(i)%n
        wrk(A(i)%i(k)) = A(i)%val(k)
      end do
      
      write(*,form) wrk
      
      
!       m = A(i)%n
!       k = 1
!       do j = 1, n
!         
!         if (i==j) then
!           write(*,'(e10.3)',advance='no') A(i)%val(1)
!         else if (k <= m) then
!           if (j==A(i)%i(k)) then
!             write(*,'(e10.3)',advance='no') A(i)%val(k+1)
!             k = k + 1
!           end if
!         else
!           write(*,'(e10.3)',advance='no') 0.000
!         end if
!         
!       end do
!       write(*,*)
    end do
    
  end subroutine prtsparse
  

  
  
  
  subroutine prtarrint(arr)
    use types
    integer, dimension(:,:), intent(in) :: arr
    
    character(len=16)  :: form
    
    integer :: i, j, n, m, k
    
    
    n = ubound(arr,1)
    write(form,'(a, i5,7a)') '(',n, 'i5)'
    
!     write(*,form) wrk
    
    
    do i = 1, n

      
      write(*,form) arr(i,:)
      

    end do
    
    
  
  end subroutine prtarrint
  
  
  subroutine prtarrreal(arr)
    use types
    real, dimension(:,:), intent(in) :: arr
    
    character(len=16)  :: form
    
    integer :: i, j, n, m, k
    
    
    n = ubound(arr,1)
    write(form,'(a, i5,7a)') '(',n, 'e12.3)'
    
!     write(*,form) wrk
    
    
    do i = 1, n

      
      write(*,form) arr(i,:)
      

    end do
    
    
  
  end subroutine prtarrreal
  
  
  
  
  
  
end module fnc