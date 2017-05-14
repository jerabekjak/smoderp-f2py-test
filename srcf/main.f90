! f90wrap -m types types.f90
! gfortran -c types.f90
! f2py -c f90wrap_types.f90 main.f90 -m main



subroutine main(mat_boundary, &
                x_coordinate, &
                y_coordinate, &
                NoDataValue, &
                array_points, &
                cols, rows,  &
                mat_pi, mat_ppl, &
                surface_retention, &
                mat_inf_index,combinatIndex, mat_hcrit, &
                mat_aa, mat_b, &
                mat_fd, mat_dmt, &
                mat_efect_vrst, mat_slope, mat_nan, &
                mat_a, mat_n, &
                pixel_area,  &
                end_time, &
                type_of_computing, &
                sr, itera, output )
  
  use types
  use fnc
!   use proces()s
  use runoff
  implicit none
  
  
  
  integer, dimension(:,:), intent(in) :: mat_boundary
  real, intent(in)                 :: x_coordinate
  real, intent(in)                 :: y_coordinate
  real, intent(in)                 :: NoDataValue
  real, dimension(:,:), intent(in) :: array_points
  integer, intent(in)              :: cols
  integer, intent(in)              :: rows
  real, dimension(:,:), intent(in) :: mat_pi
  real, dimension(:,:), intent(in) :: mat_ppl
  real, intent(in)                 :: surface_retention
  integer, dimension(:,:), intent(in) :: mat_inf_index
  real, dimension(:,:), intent(in) :: combinatIndex
  real, dimension(:,:), intent(in) :: mat_hcrit
  real, dimension(:,:), intent(in) :: mat_aa
  real, dimension(:,:), intent(in) :: mat_b
  integer, dimension(:,:), intent(in) :: mat_fd
  real, dimension(:,:), intent(in) :: mat_dmt
  real, dimension(:,:), intent(in) :: mat_efect_vrst
  real, dimension(:,:), intent(in) :: mat_slope
  real, dimension(:,:), intent(in) :: mat_nan
  real, dimension(:,:), intent(in) :: mat_a
  real, dimension(:,:), intent(in) :: mat_n
  real, intent(in)                 :: pixel_area
  real, intent(in)                 :: end_time
  integer                          :: type_of_computing
  real, dimension(:,:), intent(in) :: sr
  integer                          :: itera
  character(*)                     :: output

  
  !> pro kazdou bukdu se definuje odkud do ni tece
  type(inflowst), dimension(rows,cols) :: inflows
  type(kdepocitat) :: loop
  type(neznamat)   :: h
  !> dimension je od 0 pac v mat_inf_index jsou indexi puthnovsky od nuly
  !  k a s se pak vola infcoef(mat_inf_index(ii,jj))%k (s)
  type(infcoeft), dimension(0:(ubound(combinatIndex,1)-1)) :: infcoef
  
  
  integer :: i, j, n, ii, jj, ir, jr


  
  
  
!   do i = 1, ubound(mat_boundary,1)
!     print *, mat_boundary(i,:)
!   end do
!   do i = 1, ubound(mat_boundary,1)
!     print *, mat_fd(i,:)
!   end do
  

  

  
  !
  !
  !
  !
!   open(unit=101,file='out1.txt',status='replace',action='write')
!   open(unit=102,file='out2.txt',status='replace',action='write')
!   open(unit=103,file='out3.txt',status='replace',action='write')
  
  !
  !
  !
  
  
  
  !
  !
  !
  !  init 
  !
  !
  call make_ij(rows,cols,loop,mat_boundary)
  call make_inflows(rows,cols,inflows,mat_fd)
  call make_infiltration(combinatIndex,infcoef)
  
  h%n = loop%ntot
  allocate(h%totnew(1:h%n))
  allocate(h%totpre(1:h%n))
  allocate(h%sheet(1:h%n))
  allocate(h%rill(1:h%n))
  
  do i = 1, h%n
    h%totnew(i) = 0.0_8
    h%totpre(i) = 1.0_8
    h%sheet(i) = 1.0_8
    h%rill(i) = 0.0_8
  end do
  
  
!   print *, 'tot'
!   do i = 1, loop%ntot
!     print *, i, loop%ij(i,:)
!   end do
!   
!   print *, 'nc'
!   do i = 1, loop%n
!     j = loop%nc(i)
!     print *, j, loop%ij(j,:)
!   end do
!   
!   
!   print *, 'bcc'
!   do i = 1, loop%nbc
!     j = loop%bcc(i)
!     print *, j, loop%ij(j,:)
!   end do
!   
!   
!   print *, 
!   call prtarrint(loop%mi)
  

  
  
  !
  !
  !
  !  end init 
  !
  !
  

      
      
      


  !
  !
  !
  ! computation
  !
  !
  !

  call compute(rows, cols, end_time, &
               itera, sr, &
               loop, inflows, infcoef, h, &
               mat_aa, mat_b, mat_efect_vrst, &
               mat_inf_index,pixel_area)
  
  
  
  
  
  !
  !
  !
  ! end computation
  !
  !
  !
!   do
!     t = t + dt
!     if (t > end_time) exit
!     
!     ! dest je vsude konstantni
!     call currrain(itera, sr, t, dt, cr)
!     
!     exit_ = .false.
! 
!     ! i j cyklus
!     do i = 1, loop%n
!       ii = loop%ij(i,1)
!       jj = loop%ij(i,2)
!       runoff(ii,jj) = sheet(mat_aa(ii,jj), mat_b(ii,jj), h%totpre(i),mat_efect_vrst(ii,jj),pixel_area,dt)
!       if (runoff(ii,jj)*dt/mat_efect_vrst(ii,jj)>0.54) then
!         print *, runoff(ii,jj)*dt/mat_efect_vrst(ii,jj)
!         t  = t-dt
!         dt = 0.54*mat_efect_vrst(ii,jj)/runoff(ii,jj)
!         exit_ = .true.
!         exit
!       end if 
!     end do
!     
!     if (.not. exit_) then
! !     print *, ' '
!     ! i j cyklus
!     do i = 1, loop%n
! !     if (exit_) exit
!       ii = loop%ij(i,1)
!       jj = loop%ij(i,2)
!       
!       cin = 0.0
!       do j = 1, inflows(ii,jj)%n
!         ir = inflows(ii,jj)%in(j,1)
!         jr = inflows(ii,jj)%in(j,2)
! !         print *, '               ', ir, jr, runoff(ir,jr)
!         cin = cin + runoff(ir,jr)
!       end do
!       ks  = (/ infcoef(mat_inf_index(ii,jj))%k, infcoef(mat_inf_index(ii,jj))%s /)
! !       cinf = infiltration(ks,t,dt)
!       ! tady bez infiltrace, mozna se to totiz vse vsakne
!       h%totnew(i) = h%totpre(i) + cr + cin - runoff(ii,jj)
!       h%totnew(i) = max(0.0, h%totnew(i) - cinf)
!       cinf         = min(h%totnew(i),     cinf)
! !       print *, h%totnew(i), h%totpre(i) , cr , cin , cinf , runoff(ii,jj)
!       if ( h%totnew(i) < 0 ) ERROR stop
!     end do
!   
!     do i = 1, loop%n
! !     if (exit_) exit
!       if (i == 1) then
!         write(101,*)  t, h%totpre(i), cr, cin, cinf, runoff(ii,jj), h%totnew(i), dt/60
!       end if
!       if (i == int(loop%n/2)) then
!         write(102,*) t, h%totpre(i), cr, cin, cinf, runoff(ii,jj), h%totnew(i), dt/60
!       end if
!       if (i == loop%n) then
!         write(103,*) t, h%totpre(i), cr, cin, cinf, runoff(ii,jj), h%totnew(i), dt/60
!       end if
!       h%totpre(i) = h%totnew(i)
!       
!     end do
!     end if 
!     
! !     read(*,*)
!   end do
  
  
  
  
  
!   print *, loop%n
    
!       do i = 2, (rows-1)
!       do j = 2, (cols-1)
!         print *, inflows(i,j)%n
!         print *, inflows(i,j)%in
!       end do
!       end do
    
    
    

!   contains
  
  
  
end subroutine main



  
  
