module types

  type, public :: inflowst
    integer :: n = 0
    integer, allocatable, dimension(:) :: in
  end type inflowst

  type, public :: kdepocitat
    integer :: n = 0
    integer, allocatable, dimension(:,:) :: ij
  end type kdepocitat
  
  
end module types