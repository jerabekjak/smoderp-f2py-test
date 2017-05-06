module types

  type, public :: inflowst
    !> z kolika bunek tece do aktualni (muze byt nula)
    integer :: n = 0
    !> souradnice bunky
    integer, allocatable, dimension(:,:) :: in
  end type inflowst

  type, public :: kdepocitat
    integer :: n = 0
    integer, allocatable, dimension(:,:) :: ij
  end type kdepocitat
  
  type, public :: neznamat
    integer :: n = 0
    real, allocatable, dimension(:)    :: totNew, totPre, sheet, rill
  end type neznamat
  
  
  type, public :: infcoeft
    real :: k
    real :: s
  end type infcoeft
  
end module types