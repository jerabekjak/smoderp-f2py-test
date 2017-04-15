! Module types defined in file types.f90

subroutine f90wrap_inflowst__get__n(this, f90wrap_n)
    use types, only: inflowst
    implicit none
    type inflowst_ptr_type
        type(inflowst), pointer :: p => NULL()
    end type inflowst_ptr_type
    integer, intent(in)   :: this(2)
    type(inflowst_ptr_type) :: this_ptr
    integer, intent(out) :: f90wrap_n
    
    this_ptr = transfer(this, this_ptr)
    f90wrap_n = this_ptr%p%n
end subroutine f90wrap_inflowst__get__n

subroutine f90wrap_inflowst__set__n(this, f90wrap_n)
    use types, only: inflowst
    implicit none
    type inflowst_ptr_type
        type(inflowst), pointer :: p => NULL()
    end type inflowst_ptr_type
    integer, intent(in)   :: this(2)
    type(inflowst_ptr_type) :: this_ptr
    integer, intent(in) :: f90wrap_n
    
    this_ptr = transfer(this, this_ptr)
    this_ptr%p%n = f90wrap_n
end subroutine f90wrap_inflowst__set__n

subroutine f90wrap_inflowst__array__in(this, nd, dtype, dshape, dloc)
    use types, only: inflowst
    implicit none
    type inflowst_ptr_type
        type(inflowst), pointer :: p => NULL()
    end type inflowst_ptr_type
    integer, intent(in) :: this(2)
    type(inflowst_ptr_type) :: this_ptr
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 2
    dtype = 5
    this_ptr = transfer(this, this_ptr)
    if (allocated(this_ptr%p%in)) then
        dshape(1:2) = shape(this_ptr%p%in)
        dloc = loc(this_ptr%p%in)
    else
        dloc = 0
    end if
end subroutine f90wrap_inflowst__array__in

subroutine f90wrap_inflowst_initialise(this)
    use types, only: inflowst
    implicit none
    
    type inflowst_ptr_type
        type(inflowst), pointer :: p => NULL()
    end type inflowst_ptr_type
    type(inflowst_ptr_type) :: this_ptr
    integer, intent(out), dimension(2) :: this
    allocate(this_ptr%p)
    this = transfer(this_ptr, this)
end subroutine f90wrap_inflowst_initialise

subroutine f90wrap_inflowst_finalise(this)
    use types, only: inflowst
    implicit none
    
    type inflowst_ptr_type
        type(inflowst), pointer :: p => NULL()
    end type inflowst_ptr_type
    type(inflowst_ptr_type) :: this_ptr
    integer, intent(in), dimension(2) :: this
    this_ptr = transfer(this, this_ptr)
    deallocate(this_ptr%p)
end subroutine f90wrap_inflowst_finalise

subroutine f90wrap_kdepocitat__get__n(this, f90wrap_n)
    use types, only: kdepocitat
    implicit none
    type kdepocitat_ptr_type
        type(kdepocitat), pointer :: p => NULL()
    end type kdepocitat_ptr_type
    integer, intent(in)   :: this(2)
    type(kdepocitat_ptr_type) :: this_ptr
    integer, intent(out) :: f90wrap_n
    
    this_ptr = transfer(this, this_ptr)
    f90wrap_n = this_ptr%p%n
end subroutine f90wrap_kdepocitat__get__n

subroutine f90wrap_kdepocitat__set__n(this, f90wrap_n)
    use types, only: kdepocitat
    implicit none
    type kdepocitat_ptr_type
        type(kdepocitat), pointer :: p => NULL()
    end type kdepocitat_ptr_type
    integer, intent(in)   :: this(2)
    type(kdepocitat_ptr_type) :: this_ptr
    integer, intent(in) :: f90wrap_n
    
    this_ptr = transfer(this, this_ptr)
    this_ptr%p%n = f90wrap_n
end subroutine f90wrap_kdepocitat__set__n

subroutine f90wrap_kdepocitat__array__ij(this, nd, dtype, dshape, dloc)
    use types, only: kdepocitat
    implicit none
    type kdepocitat_ptr_type
        type(kdepocitat), pointer :: p => NULL()
    end type kdepocitat_ptr_type
    integer, intent(in) :: this(2)
    type(kdepocitat_ptr_type) :: this_ptr
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 2
    dtype = 5
    this_ptr = transfer(this, this_ptr)
    if (allocated(this_ptr%p%ij)) then
        dshape(1:2) = shape(this_ptr%p%ij)
        dloc = loc(this_ptr%p%ij)
    else
        dloc = 0
    end if
end subroutine f90wrap_kdepocitat__array__ij

subroutine f90wrap_kdepocitat_initialise(this)
    use types, only: kdepocitat
    implicit none
    
    type kdepocitat_ptr_type
        type(kdepocitat), pointer :: p => NULL()
    end type kdepocitat_ptr_type
    type(kdepocitat_ptr_type) :: this_ptr
    integer, intent(out), dimension(2) :: this
    allocate(this_ptr%p)
    this = transfer(this_ptr, this)
end subroutine f90wrap_kdepocitat_initialise

subroutine f90wrap_kdepocitat_finalise(this)
    use types, only: kdepocitat
    implicit none
    
    type kdepocitat_ptr_type
        type(kdepocitat), pointer :: p => NULL()
    end type kdepocitat_ptr_type
    type(kdepocitat_ptr_type) :: this_ptr
    integer, intent(in), dimension(2) :: this
    this_ptr = transfer(this, this_ptr)
    deallocate(this_ptr%p)
end subroutine f90wrap_kdepocitat_finalise

subroutine f90wrap_neznamat__get__n(this, f90wrap_n)
    use types, only: neznamat
    implicit none
    type neznamat_ptr_type
        type(neznamat), pointer :: p => NULL()
    end type neznamat_ptr_type
    integer, intent(in)   :: this(2)
    type(neznamat_ptr_type) :: this_ptr
    integer, intent(out) :: f90wrap_n
    
    this_ptr = transfer(this, this_ptr)
    f90wrap_n = this_ptr%p%n
end subroutine f90wrap_neznamat__get__n

subroutine f90wrap_neznamat__set__n(this, f90wrap_n)
    use types, only: neznamat
    implicit none
    type neznamat_ptr_type
        type(neznamat), pointer :: p => NULL()
    end type neznamat_ptr_type
    integer, intent(in)   :: this(2)
    type(neznamat_ptr_type) :: this_ptr
    integer, intent(in) :: f90wrap_n
    
    this_ptr = transfer(this, this_ptr)
    this_ptr%p%n = f90wrap_n
end subroutine f90wrap_neznamat__set__n

subroutine f90wrap_neznamat__array__totnew(this, nd, dtype, dshape, dloc)
    use types, only: neznamat
    implicit none
    type neznamat_ptr_type
        type(neznamat), pointer :: p => NULL()
    end type neznamat_ptr_type
    integer, intent(in) :: this(2)
    type(neznamat_ptr_type) :: this_ptr
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 1
    dtype = 11
    this_ptr = transfer(this, this_ptr)
    if (allocated(this_ptr%p%totnew)) then
        dshape(1:1) = shape(this_ptr%p%totnew)
        dloc = loc(this_ptr%p%totnew)
    else
        dloc = 0
    end if
end subroutine f90wrap_neznamat__array__totnew

subroutine f90wrap_neznamat__array__totpre(this, nd, dtype, dshape, dloc)
    use types, only: neznamat
    implicit none
    type neznamat_ptr_type
        type(neznamat), pointer :: p => NULL()
    end type neznamat_ptr_type
    integer, intent(in) :: this(2)
    type(neznamat_ptr_type) :: this_ptr
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 1
    dtype = 11
    this_ptr = transfer(this, this_ptr)
    if (allocated(this_ptr%p%totpre)) then
        dshape(1:1) = shape(this_ptr%p%totpre)
        dloc = loc(this_ptr%p%totpre)
    else
        dloc = 0
    end if
end subroutine f90wrap_neznamat__array__totpre

subroutine f90wrap_neznamat__array__sheet(this, nd, dtype, dshape, dloc)
    use types, only: neznamat
    implicit none
    type neznamat_ptr_type
        type(neznamat), pointer :: p => NULL()
    end type neznamat_ptr_type
    integer, intent(in) :: this(2)
    type(neznamat_ptr_type) :: this_ptr
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 1
    dtype = 11
    this_ptr = transfer(this, this_ptr)
    if (allocated(this_ptr%p%sheet)) then
        dshape(1:1) = shape(this_ptr%p%sheet)
        dloc = loc(this_ptr%p%sheet)
    else
        dloc = 0
    end if
end subroutine f90wrap_neznamat__array__sheet

subroutine f90wrap_neznamat__array__rill(this, nd, dtype, dshape, dloc)
    use types, only: neznamat
    implicit none
    type neznamat_ptr_type
        type(neznamat), pointer :: p => NULL()
    end type neznamat_ptr_type
    integer, intent(in) :: this(2)
    type(neznamat_ptr_type) :: this_ptr
    integer, intent(out) :: nd
    integer, intent(out) :: dtype
    integer, dimension(10), intent(out) :: dshape
    integer*8, intent(out) :: dloc
    
    nd = 1
    dtype = 11
    this_ptr = transfer(this, this_ptr)
    if (allocated(this_ptr%p%rill)) then
        dshape(1:1) = shape(this_ptr%p%rill)
        dloc = loc(this_ptr%p%rill)
    else
        dloc = 0
    end if
end subroutine f90wrap_neznamat__array__rill

subroutine f90wrap_neznamat_initialise(this)
    use types, only: neznamat
    implicit none
    
    type neznamat_ptr_type
        type(neznamat), pointer :: p => NULL()
    end type neznamat_ptr_type
    type(neznamat_ptr_type) :: this_ptr
    integer, intent(out), dimension(2) :: this
    allocate(this_ptr%p)
    this = transfer(this_ptr, this)
end subroutine f90wrap_neznamat_initialise

subroutine f90wrap_neznamat_finalise(this)
    use types, only: neznamat
    implicit none
    
    type neznamat_ptr_type
        type(neznamat), pointer :: p => NULL()
    end type neznamat_ptr_type
    type(neznamat_ptr_type) :: this_ptr
    integer, intent(in), dimension(2) :: this
    this_ptr = transfer(this, this_ptr)
    deallocate(this_ptr%p)
end subroutine f90wrap_neznamat_finalise

! End of module types defined in file types.f90

