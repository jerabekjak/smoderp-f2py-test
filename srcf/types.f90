module types


  ! kind jednak nejde jako parameter
  ! druhak real jde z pythonu defaultne kind=4
  ! integer, parameter, public :: rk = selected_real_kind(16,32)
  ! integer, parameter, public :: rk = 8

  type, public :: inflowst
    !> z kolika bunek tece do aktualni (muze byt nula)
    integer :: n = 0
    !> souradnice bunky
    integer, allocatable, dimension(:,:) :: in
  end type inflowst

  type, public :: kdepocitat
    integer :: n    = 0
    !> nornam cell
    integer, allocatable, dimension(:)   :: nc
    integer :: nbc  = 0
    !> boundary condition cell
    integer, allocatable, dimension(:)   :: bcc
    integer :: ntot = 0
    integer, allocatable, dimension(:,:) :: ij
!     integer, allocatable, dimension(:,:) :: ijbc
    ! stores position in ij and ijbc
    integer, allocatable, dimension(:,:) :: mi
  end type kdepocitat
  
  type, public :: neznamat
    integer :: n = 0
    real, allocatable, dimension(:)    :: totNew, totPre, sheet, rill
  end type neznamat
  
  
  type, public :: infcoeft
    real :: k
    real :: s
  end type infcoeft
  
  
  
  
  
  
  
  
  type, public :: sparsematt
    ! pozice na radku
    integer, dimension(1:9) :: i = 0
    ! n je kolik je plnych na radku
    ! minumalne je 1
    integer                 :: n = 0
    ! na cislo na pozici
    real   , dimension(1:9) :: val = 0
  end type sparsematt
  
  
end module types