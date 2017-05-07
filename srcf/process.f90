module process

 contains
  
  
  !> zatim funguje jen pro krabici a nedela zbytek 
  !  na rozhladni t a t+dt
  !  vraci vysku
  subroutine currrain(it, sr, t, dt, cr)
    integer, intent(inout)  :: it
    real, dimension(:,:)    :: sr
    real, intent(in)        :: t
    real, intent(in)        :: dt
    real, intent(out)     :: cr
    
    if (t > sr(it,1)) then
      cr = 0
      return
!       it = it + 1
    end if 
    
    cr = sr(it,2)*dt
    
    
  end subroutine currrain
  
  
  !>  vraci vysku
  function infiltration(ks, t, dt) result(inf)
    real, dimension(1:2), intent(in) :: ks
    real, intent(in)     :: t
    real, intent(in)     :: dt
    real                 :: inf
    
    real :: k
    real :: s
    
    k = ks(1)
    s = ks(2)
    
  
    inf = (0.5*s/sqrt(t+dt) + k) * dt
  
  end function infiltration
  
  
  
  !> q je specificky prutok
  ! na V se prevede jako dt * q * dx
  ! vraci vysku
  function sheet(a, b, h, ef, pixel,dt) result(q)
    real, intent(in)     :: a
    real, intent(in)     :: b
    real, intent(in)     :: h
    real, intent(in)     :: ef
    real, intent(in)     :: pixel
    real, intent(in)     :: dt
    real                 :: q 
    

    q = a*h**b*ef*dt/pixel
    
  end function sheet
  
  
  
end module process