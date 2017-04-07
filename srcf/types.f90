module types

  type, public :: inflowst
    integer :: n = 0
    integer, allocatable, dimension(:) :: in
  end type inflowst
  

end module types