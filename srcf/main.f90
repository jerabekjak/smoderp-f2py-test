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
                mat_inf_index, mat_hcrit, &
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
  implicit none
  
  
  
  integer, dimension(:,:), intent(in) :: mat_boundary
  real, intent(in)                 :: x_coordinate
  real, intent(in)                 ::  y_coordinate
  real, intent(in)                 :: NoDataValue
  real, dimension(:,:), intent(in) :: array_points
  integer, intent(in)              :: cols
  integer, intent(in)              :: rows
  real, dimension(:,:), intent(in) :: mat_pi
  real, dimension(:,:), intent(in) :: mat_ppl
  real, intent(in)                 :: surface_retention
  real, dimension(:,:), intent(in) :: mat_inf_index
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

  
  type(inflowst), dimension(rows,cols) :: inflows
  type(kdepocitat) :: loop
  
! 
  integer :: i, j
! !   
!   do i = 1, rows
!   
!     print *, mat_boundary(i,:)
! 
!   end do

  
  
!   
  call make_ij(rows,cols,loop,mat_boundary)
  
  print *, loop%n
  do i = 1, loop%n
    print *, loop%ij(i,:)
  end do
    
    
    

!   contains
  
  
  
end subroutine main



  
  
