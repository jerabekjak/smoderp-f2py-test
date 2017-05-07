module runoff
 contains

 
  subroutine compute( r, c, end_time, &
                      itera, sr, &
                      loop, inflows, infcoef, h, &
                      mat_aa, mat_b, mat_efect_vrst,&
                      mat_inf_index,pixel_area)
    use types
    use fnc
    implicit none
    integer, intent(in)    :: r
    integer, intent(in)    :: c
    real, intent(in)       :: end_time
    integer, intent(inout)    :: itera
    real, dimension(:,:), intent(in)           :: sr
    type(kdepocitat), intent(in)               :: loop
    type(inflowst), dimension(:,:), intent(in) :: inflows
    type(infcoeft), dimension(0:),  intent(in) :: infcoef
    type(neznamat), intent(inout)    :: h
    real, dimension(:,:), intent(in) :: mat_aa
    real, dimension(:,:), intent(in) :: mat_b
    real, dimension(:,:), intent(in) :: mat_efect_vrst
    integer, dimension(:,:), intent(in) :: mat_inf_index
    real, intent(in) :: pixel_area
    
    ! local variables
    real :: cr !> current rain head
    real :: cin !> current inflow head
    real :: cinf !> current infiltration head
    real :: t
    real :: dt
    real, dimension(1:2) :: ks  !> current infiltration parameters
    logical :: exit_
    integer :: i, j, n, ii, jj, ir, jr
    
    type(sparsematt), dimension(loop%n) :: A
    real, dimension(loop%n)           :: b
    
    
    ! set sparse matrix to zero
    
    
    t  = 0.
  !   print *, 'dt [s]:'
  !   read  *,  dt
    dt = 10.

    main: do
    
      do i = 1, loop%n
        A(i)%i   = -1
        A(i)%val = 0.
      end do
    
    
    
      t = t + dt
      if (t > end_time) exit
!       print *, t
      
      call fillsystem(r,c,mat_aa,mat_b,mat_efect_vrst,pixel_area,dt,loop,inflows,h%totpre,A,b)
      
      call prtsparse(loop%n,A)
      
      
      
      !
      !
      !
      !
      stop
      !
      !
      !
      !
      
      
    end do main
    
    
  end subroutine compute



  subroutine fillsystem(r,c,mat_aa,mat_b,mat_efect_vrst,pixel_area,dt,loop,inflows,x,A,b)
    use types
    use process
    integer, intent(in)              :: r,c
    real, dimension(:,:), intent(in) :: mat_aa
    real, dimension(:,:), intent(in) :: mat_b
    real, dimension(:,:), intent(in) :: mat_efect_vrst
    real, intent(in)                 :: pixel_area
    real, intent(in)                 :: dt
    type(kdepocitat), intent(in)     :: loop
    type(inflowst), dimension(:,:), intent(in) :: inflows
    real, dimension(:), intent(in)   :: x
    type(sparsematt), dimension(loop%n), intent(out) :: A
    real, dimension(loop%n),        intent(out) :: b
    
    integer :: i, j, ii, jj
    real, dimension(r,c) :: roff
!     real :: cin
    
    roff = 0.
    
    
    
    
    
    
    
    ! get runoff
    do i = 1, loop%n
      ii = loop%ij(i,1)
      jj = loop%ij(i,2)
      roff(ii,jj) = sheet(mat_aa(ii,jj), mat_b(ii,jj), x(i),mat_efect_vrst(ii,jj),pixel_area,dt)
    end do
    
    
    do i = 1, loop%n
    
      A(ii)%n      = 0
      
      ii = loop%ij(i,1)
      jj = loop%ij(i,2)
      
      A(i)%val(1) = roff(ii,jj)
      A(i)%i(1)   = jj
      A(i)%n      = 1
      
      
      
!       cin = 0.0
      do j = 1, inflows(ii,jj)%n
        ir = inflows(ii,jj)%in(j,1)
        jr = inflows(ii,jj)%in(j,2)
        A(ii)%val(j+1) = roff(ir,jr)
        A(ii)%i(j+1)   = jr
        A(ii)%n        = A(ii)%n + 1
!         cin = cin + roff(ir,jr)
      end do
      
      
    end do
    
    
    
  
  
  end subroutine
  
  
  
  
  subroutine kontrolaAbx()
    use types
    use fnc
    implicit none
    type(sparsematt), dimension(10) :: A
    real, dimension(10)             :: x, b
    
    integer :: i, k
    

    
    A(1)%val(1) = -2.
    A(1)%val(2) = 1.
    A(1)%val(3) = 2.
    A(1)%i(1) = 1
    A(1)%i(2) = 2
    A(1)%i(3) = 5
    A(1)%n    = 3
      
    b = (/10.,1.,4.,9.,1.,3.,5.,2.,3.,4./)
    x = 0.
    
    do i = 1, 10
      print *, A(i)%n
!       print *, A(i)%i
!       print *, A(i)%val
      do k = 1, A(i)%n
        print *, b(A(i)%i(k)), A(i)%val(k)
        x(i) = x(i) + b(A(i)%i(k))*A(i)%val(k)
      end do
    end do
    
      
    
!     call prtsparse(10,A)
    print *, x
  
  end subroutine kontrolaAbx
  
  
  
end  module runoff







!         ! dest je vsude konstantni
!         call currrain(itera, sr, t, dt, cr)
!         
!         exit_ = .false.
! 
!         ! i j cyklus
!         do i = 1, loop%n
!           ii = loop%ij(i,1)
!           jj = loop%ij(i,2)
!           roff(ii,jj) = sheet(mat_aa(ii,jj), mat_b(ii,jj), h%totpre(i),mat_efect_vrst(ii,jj),pixel_area,dt)
!           if (roff(ii,jj)*dt/mat_efect_vrst(ii,jj)>0.54) then
!             print *, roff(ii,jj)*dt/mat_efect_vrst(ii,jj)
!             t  = t-dt
!             dt = 0.54*mat_efect_vrst(ii,jj)/roff(ii,jj)
!             exit_ = .true.
!             exit
!           end if 
!         end do
!         
!         if (.not. exit_) then
!     !     print *, ' '
!         ! i j cyklus
!         do i = 1, loop%n
!     !     if (exit_) exit
!           ii = loop%ij(i,1)
!           jj = loop%ij(i,2)
!           
!           cin = 0.0
!           do j = 1, inflows(ii,jj)%n
!             ir = inflows(ii,jj)%in(j,1)
!             jr = inflows(ii,jj)%in(j,2)
!     !         print *, '               ', ir, jr, roff(ir,jr)
!             cin = cin + roff(ir,jr)
!           end do
!           ks  = (/ infcoef(mat_inf_index(ii,jj))%k, infcoef(mat_inf_index(ii,jj))%s /)
!     !       cinf = infiltration(ks,t,dt)
!           ! tady bez infiltrace, mozna se to totiz vse vsakne
!           h%totnew(i) = h%totpre(i) + cr + cin - roff(ii,jj)
!           h%totnew(i) = max(0.0, h%totnew(i) - cinf)
!           cinf         = min(h%totnew(i),     cinf)
!     !       print *, h%totnew(i), h%totpre(i) , cr , cin , cinf , roff(ii,jj)
!           if ( h%totnew(i) < 0 ) ERROR stop
!         end do
!       
!         do i = 1, loop%n
!     !     if (exit_) exit
!           if (i == 1) then
!             write(101,*)  t, h%totpre(i), cr, cin, cinf, roff(ii,jj), h%totnew(i), dt/60
!           end if
!           if (i == int(loop%n/2)) then
!             write(102,*) t, h%totpre(i), cr, cin, cinf, roff(ii,jj), h%totnew(i), dt/60
!           end if
!           if (i == loop%n) then
!             write(103,*) t, h%totpre(i), cr, cin, cinf, roff(ii,jj), h%totnew(i), dt/60
!           end if
!           h%totpre(i) = h%totnew(i)
!           
!         end do
!         end if 
!         
!     !     read(*,*)