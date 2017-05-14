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
    
    type(sparsematt), dimension(loop%ntot) :: A
    real, dimension(loop%ntot)           :: b, hguess
    
    
    ! set sparse matrix to zero
    !
    !
    !
    !
    call kontrolasolve()
    !
    !
          stop
    !
    !
    !
    !
    !
    !
    t  = 0.
  !   print *, 'dt [s]:'
  !   read  *,  dt
    dt = 10.
    
    hguess = h%totpre
    
    main: do
    
      do i = 1, loop%n
        A(i)%i   = -1
        A(i)%val =  0.
        A(i)%n   =  0
      end do
    
    
    
      t = t + dt
      if (t > end_time) exit
!       print *, t
      
      do
        call fillsystem(r,c,mat_aa,mat_b,mat_efect_vrst,pixel_area,t,dt,loop,inflows,h%totpre,hguess,infcoef,  &
                        mat_inf_index,itera,sr,A,b)
        h%totnew  =  h%totpre 
        call solve(loop%ntot,h%totnew, A, b)
        if (sum(abs(h%totnew - hguess)) < 0.02) exit
        
        hguess = h%totnew
      end do
!       call prtsparse(loop%ntot,A)
!       
!       
!       print *, b
      
      
        !
      !
      !
      !

      !
      !
      !
      !
      
      
    end do main
    
    
  end subroutine compute



  subroutine fillsystem(r,c,mat_aa,mat_b,mat_efect_vrst,pixel_area,t,dt,loop,inflows,x,hguess,infcoef,mat_inf_index,it, sr,A,b)
    use types
    use process
    use fnc
    implicit none
    integer, intent(in)              :: r,c
    real, dimension(:,:), intent(in) :: mat_aa
    real, dimension(:,:), intent(in) :: mat_b
    real, dimension(:,:), intent(in) :: mat_efect_vrst
    real, intent(in)                 :: pixel_area
    real, intent(in)                 ::  t
    real, intent(in)                 :: dt
    type(kdepocitat), intent(in)     :: loop
    type(inflowst), dimension(:,:), intent(in) :: inflows
    real, dimension(:), intent(in)   :: x
    real, dimension(:), intent(in)   :: hguess
    type(infcoeft), dimension(0:),  intent(in) :: infcoef
    integer, dimension(:,:), intent(in) :: mat_inf_index
    integer, intent(inout)    :: it
    real, dimension(:,:), intent(in)           :: sr
    type(sparsematt), dimension(loop%ntot), intent(out) :: A
    real, dimension(loop%ntot),        intent(out) :: b
    
    integer :: i, j, ii, jj, ir, jr, ic
    real    :: k, s
    real, dimension(r,c) :: roff
!     real :: cin
    
    roff = 0.
    

    
    
    ! get runoff
    do i = 1, loop%ntot
      ii = loop%ij(i,1)
      jj = loop%ij(i,2)
      roff(ii,jj) = sheet(mat_aa(ii,jj), mat_b(ii,jj)-1.0, hguess(i),mat_efect_vrst(ii,jj),pixel_area) * dt
    end do
    
!     call prtarrreal(roff)
    
    do i = 1, loop%ntot
    
      A(i)%n      = 0
      
      ii = loop%ij(i,1)
      jj = loop%ij(i,2)
      
      
      ! diagonal musi byt na prvnim myste v vektoru A(*)%i
      ! jinak nefunguje solver
      A(i)%val(1) = roff(ii,jj)
      A(i)%i(1)   = i
      A(i)%n      = 1
      
      do j = 1, inflows(ii,jj)%n
        ir = inflows(ii,jj)%in(j,1)
        jr = inflows(ii,jj)%in(j,2)
        A(ii)%val(j+1) = roff(ir,jr)
        A(ii)%i(j+1)   = loop%mi(ir,jr)
        A(ii)%n        = A(ii)%n + 1
      end do
      
      
      ic = mat_inf_index(ii,jj)
      k  = infcoef(ic)%k
      s  = infcoef(ic)%s
      b(i) = x(i) + infiltration((/k,s/),t) * dt + currrain(it, sr, t) * dt
    end do
    
    
    
  
  
  end subroutine fillsystem
  
  
  
  
  
  subroutine solve(n,h,A,b)
    use types
    implicit none
    integer, intent(in) :: n
    real, dimension(n),        intent(inout) :: h
    type(sparsematt), dimension(n), intent(in) :: A
    real, dimension(n),        intent(in) :: b
    real  :: sigma
    real, dimension(size(h)) :: hcheck
    integer :: i, j, k
    
    
    do
      hcheck = h
      do i = 1, n 
        sigma = 0.0
        do k = 2,A(i)%n
          j = A(i)%i(k)
!           print *, j, A(i)%val(k)
          sigma = sigma + A(i)%val(k)*h(j)
        end do
        k = 1 
!         print *, A(i)%val(k)
        h(i) = 1./A(i)%val(k)*(b(i)-sigma)
      end do
      print *, 
      print *, hcheck
      print *, h
!       read(*,*)
      if (sum(abs(hcheck-h)) < 0.00002) exit
    end do
    
  
  end subroutine
  
  subroutine kontrolasolve()
    use types
    use fnc
    implicit none
    type(sparsematt), dimension(4) :: A
    real, dimension(4)             :: x, b
    
    integer :: i, k
    
    b = 0.
    x = 0.
    
    k = 1
    A(k)%val(1) = 10.
    A(k)%val(2) = -1.
    A(k)%val(3) = 2.
    A(k)%i(1) = 1
    A(k)%i(2) = 2
    A(k)%i(3) = 3
    A(k)%n    = 3
    b(k)      = 6.
      
    k = 2
    A(k)%val(1) = 11.
    A(k)%val(2) = -1.
    A(k)%val(3) = -1.
    A(k)%val(4) = 3.
    A(k)%i(1) = 2
    A(k)%i(2) = 1
    A(k)%i(3) = 3
    A(k)%i(4) = 4
    A(k)%n    = 4
    b(k)      = 25.
      
      
    k = 3
    A(k)%val(1) = 10.
    A(k)%val(2) = 2.
    A(k)%val(3) = -1.
    A(k)%val(4) = -1.
    A(k)%i(1) = 3
    A(k)%i(2) = 1
    A(k)%i(3) = 2
    A(k)%i(4) = 4
    A(k)%n    = 4
    b(k)      = -11.
    
    
    k = 4
    A(k)%val(1) = 8.
    A(k)%val(2) = 3.
    A(k)%val(3) = -1.
    A(k)%i(1) = 4
    A(k)%i(2) = 2
    A(k)%i(3) = 3
    A(k)%n    = 3
    b(k)      = 15.
    
    
    
    call prtsparse(4,A)
     
    
      
    
    print *, b
    
    
    call solve(4,x,A,b)
  
  end subroutine kontrolasolve
  
  
end  module runoff



!   subroutine kontrolaAbx()
!     use types
!     use fnc
!     implicit none
!     type(sparsematt), dimension(10) :: A
!     real, dimension(10)             :: x, b
!     
!     integer :: i, k
!     
! 
!     
!     A(1)%val(1) = -2.
!     A(1)%val(2) = 1.
!     A(1)%val(3) = 2.
!     A(1)%i(1) = 1
!     A(1)%i(2) = 2
!     A(1)%i(3) = 5
!     A(1)%n    = 3
!       
!     b = (/10.,1.,4.,9.,1.,3.,5.,2.,3.,4./)
!     x = 0.
!     
!     do i = 1, 10
!       print *, A(i)%n
! !       print *, A(i)%i
! !       print *, A(i)%val
!       do k = 1, A(i)%n
!         print *, b(A(i)%i(k)), A(i)%val(k)
!         x(i) = x(i) + b(A(i)%i(k))*A(i)%val(k)
!       end do
!     end do
!     
!       
!     
! !     call prtsparse(10,A)
!     print *, x
!   
!   end subroutine kontrolaAbx




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