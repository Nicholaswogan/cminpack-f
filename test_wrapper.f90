module test_wrapper
  use, intrinsic :: iso_c_binding, only: c_ptr, c_loc, c_f_pointer
  use, intrinsic :: iso_fortran_env, only: dp => real64
  implicit none
  
  type f_data
    real(dp) :: a
    real(dp) :: b
  end type
  
contains
  
  integer function test_func1(p, n, x, fvec, iflag) result(res) bind(c)
    type(c_ptr) :: p
    integer, value :: n
    real(dp), intent(in) :: x(n)
    real(dp), intent(out) :: fvec(n)
    integer, value :: iflag
    type(f_data), pointer :: params
    
    call c_f_pointer(p, params) ! dereference pointer
    fvec(1) = x(1)**2.0_dp - (params%a + params%b)
    
    res = 0
  end function
  
  integer function test_func2(p, n, x, fvec, iflag) result(res) bind(c)
    type(c_ptr) :: p
    integer, value :: n
    real(dp), intent(in) :: x(n)
    real(dp), intent(out) :: fvec(n)
    integer, value :: iflag
    
    real(dp), pointer :: params
    
    call c_f_pointer(p, params) ! dereference pointer
    fvec(1) = x(1)**2.0_dp - params
    
    res = 0
  end function
  
  integer function test_func3(p, m, n, x, fvec, iflag) result(res) bind(c)
    type(c_ptr) :: p
    integer, value :: m, n
    real(dp), intent(in) :: x(n)
    real(dp), intent(out) :: fvec(m)
    integer, value :: iflag
    
    fvec(1) = x(1)**2.0_dp - 17.0_dp
    
    res = 0
  end function
  
  subroutine test_hybrd1()
    use cminpack_wrapper
    use, intrinsic :: iso_c_binding, only : c_loc, c_ptr
    implicit none

    type(f_data), target :: params1
    real(dp), target :: params2
    type(c_ptr), target :: p1, p2
    integer :: n = 1
    real(dp) :: x(1)
    real(dp) :: fvec(1)
    real(dp) :: tol = 1.0e-8_dp
    integer :: lwa = (1*(3*1+13))/2+2
    integer :: info
    real(dp) :: wa((1*(3*1+13))/2+2)

    x = [10.0_dp]
    params1%a = 5.0_dp
    params1%b = 5.0_dp
    p1 = c_loc(params1) ! get pointer
    ! pass pointer to hybrd1
    call hybrd1(test_func1, p1, n, x, fvec, tol, info, wa, lwa)
    
    call check("hybrd1 (1)",x(1), sqrt(10.0_dp))
    
    x = [10.0_dp]
    params2 = 17.0_dp
    p2 = c_loc(params2) ! get pointer
    call hybrd1(test_func2, p2, n, x, fvec, tol, info, wa, lwa)
    
    call check("hybrd1 (2)",x(1), sqrt(17.0_dp))
    
  end subroutine
  
  subroutine test_lmdif1()
    use cminpack_wrapper
    use, intrinsic :: iso_c_binding, only : c_loc, c_ptr
    implicit none

    type(c_ptr), target :: p1
    integer, parameter :: n = 1
    integer, parameter :: m = 1
    real(dp) :: x(n)
    real(dp) :: fvec(m)
    real(dp) :: tol = 1.0e-8_dp
    integer :: iwa(n)
    integer, parameter :: lwa = m*n+5*n+m + 2
    integer :: info
    real(dp) :: wa(lwa)
    
    x = [10.0_dp]
    call lmdif1(test_func3, p1, m, n, x, fvec, tol, info, iwa, wa, lwa)
    
    call check("lmdif1",x(1), sqrt(17.0_dp))
    
  end subroutine
  
  subroutine check(name, a, ans)
    real(dp), intent(in) :: a, ans
    character(len=*), intent(in) :: name
    
    real(dp), parameter :: tol = 1.0e-5_dp
    character(len=:), allocatable :: msg
    
    if (a < ans-abs(tol*ans) .or. a > ans+abs(tol*ans)) then
      msg = 'Test "'//name//'" failed.'
      error stop msg
    endif
    
  end subroutine
  
end module

program main
  use test_wrapper, only: test_hybrd1, test_lmdif1
  implicit none

  call test_hybrd1()
  call test_lmdif1()
  print*, "All tests passed!"

end program
