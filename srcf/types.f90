module types

  type, public :: inflowst
    integer :: n = 0
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
  
  
  
  
end module types