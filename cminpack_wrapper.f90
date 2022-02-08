
module cminpack_wrapper
  use, intrinsic :: iso_c_binding, only : c_int, c_double, c_ptr, c_funptr, c_funloc
  implicit none
  private
  
  public :: hybrd1, lmdif1
  
  interface
    subroutine hybrd1f_wrapper(funcptr, p, n, x, fvec, tol, info, wa, lwa) bind(C, name='hybrd1c_wrapper')
      use, intrinsic :: iso_c_binding, only : c_int, c_double, c_ptr, c_funptr
      implicit none
      type(c_funptr), value :: funcptr
      type(c_ptr) :: p
      integer(c_int) :: n
      real(c_double) :: x(n)
      real(c_double) :: fvec(n)
      real(c_double) :: tol
      integer(c_int) :: info 
      real(c_double) :: wa(lwa)
      integer(c_int) :: lwa  
    end subroutine
    
    subroutine lmdif1f_wrapper(funcptr, p, m, n, x, fvec, tol, info, iwa, wa, lwa) bind(C, name='lmdif1c_wrapper')
      use, intrinsic :: iso_c_binding, only : c_int, c_double, c_ptr, c_funptr
      implicit none
      type(c_funptr), value :: funcptr
      type(c_ptr) :: p
      integer(c_int) :: m
      integer(c_int) :: n
      real(c_double) :: x(n)
      real(c_double) :: fvec(n)
      real(c_double) :: tol
      integer(c_int) :: info 
      integer(c_int) :: iwa(n)
      real(c_double) :: wa(lwa)
      integer(c_int) :: lwa  
    end subroutine
  end interface
  
  abstract interface
    function hybrd1_callback(p, n, x, fvec, iflag) result(res) bind(c)
      use, intrinsic :: iso_c_binding, only : c_int, c_double, c_ptr
      type(c_ptr) :: p
      integer(c_int), value :: n
      real(c_double), intent(in) :: x(n)
      real(c_double), intent(out) :: fvec(n)
      integer(c_int), value :: iflag
      
      integer(c_int) :: res
    end function
    
    function lmdif1_callback(p, m, n, x, fvec, iflag) result(res) bind(c)
      use, intrinsic :: iso_c_binding, only : c_int, c_double, c_ptr
      type(c_ptr) :: p
      integer(c_int), value :: m, n
      real(c_double), intent(in) :: x(n)
      real(c_double), intent(out) :: fvec(m)
      integer(c_int), value :: iflag
      
      integer(c_int) :: res
    end function
  end interface

contains

  subroutine hybrd1(func, p, n, x, fvec, tol, info, wa, lwa)
    procedure(hybrd1_callback) :: func
    type(c_ptr), intent(in) :: p
    integer(c_int), intent(in) :: n
    real(c_double), intent(inout) :: x(n)
    real(c_double), intent(inout) :: fvec(n)
    real(c_double), intent(in) :: tol
    integer(c_int), intent(inout) :: info
    real(c_double), intent(inout) :: wa(lwa)
    integer(c_int), intent(in) :: lwa
    type (c_funptr) :: funcptr
    funcptr = c_funloc(func)    
    call hybrd1f_wrapper(funcptr, p, n, x, fvec, tol, info, wa, lwa)
  end subroutine
  
  subroutine lmdif1(func, p, m, n, x, fvec, tol, info, iwa, wa, lwa)
    procedure(lmdif1_callback) :: func
    type(c_ptr), intent(in) :: p
    integer(c_int), intent(in) :: m, n
    real(c_double), intent(inout) :: x(n)
    real(c_double), intent(inout) :: fvec(n)
    real(c_double), intent(in) :: tol
    integer(c_int), intent(inout) :: info
    integer(c_int), intent(inout) :: iwa(n)
    real(c_double), intent(inout) :: wa(lwa)
    integer(c_int), intent(in) :: lwa
    type (c_funptr) :: funcptr
    funcptr = c_funloc(func)    
    call lmdif1f_wrapper(funcptr, p, m, n, x, fvec, tol, info, iwa, wa, lwa)
  end subroutine

end module





