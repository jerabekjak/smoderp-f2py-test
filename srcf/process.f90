module process

 contains
  
  
  !> zatim funguje jen pro krabici a nedela zbytek 
  !  na rozhladni t a t+dt
  !  vraci vysku
  function currrain(it, sr, t) result(cr)
    use types
    integer, intent(inout)  :: it
    real, dimension(:,:)    :: sr
    real, intent(in)        :: t
    real    :: cr
    
    if (t > sr(it,1)) then
      cr = 0
      return
!       it = it + 1
    end if 
    
    cr = sr(it,2)
    
    
  end function currrain
  
  
  !>  vraci intezitu
  function infiltration(ks, t) result(inf)
    use types
    real, dimension(1:2), intent(in) :: ks
    real, intent(in)     :: t
    real                 :: inf
    
    real :: k
    real :: s
    
    k = ks(1)
    s = ks(2)
    
  
    inf = (0.5*s/sqrt(t+dt) + k) 
  
  end function infiltration
  
  
  
  !> q je specificky prutok
  ! na V se prevede jako dt * q * dx
  ! vraci vysku
  function sheet(a, b, h, ef, pixel) result(q)
    use types
    real, intent(in)     :: a
    real, intent(in)     :: b
    real, intent(in)     :: h
    real, intent(in)     :: ef
    real, intent(in)     :: pixel
    real                 :: q 
    

    q = a*h**b*ef/pixel
    
    
  end function sheet
  
  
  
end module process