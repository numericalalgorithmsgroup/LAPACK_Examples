    Function nagf_blas_dpyth(a, b)
!
!     Mark 12 Release. NAG Copyright 1986.
!
!     NAGF_BLAS_DPYTH returns the value
!
!        p = sqrt( a*a + b*b )
!
!     via the function name.
!
!
!     Nag Fortran 77 O( 1 ) basic linear algebra routine.
!
!     -- Written on 17-January-1985.
!     Sven Hammarling, Nag Central Office.
!

!     .. Use Statements ..
      Use lapack_precision, Only: dp
!     .. Implicit None Statement ..
      Implicit None
!     .. Function Return Value ..
      Real (Kind=dp) :: nagf_blas_dpyth
!     .. Parameters ..
      Real (Kind=dp), Parameter :: zero = 0.0E+0_dp
!     .. Scalar Arguments ..
      Real (Kind=dp) :: a, b
!     .. Local Scalars ..
      Real (Kind=dp) :: p
!     .. Intrinsic Procedures ..
      Intrinsic :: abs, sqrt
!     .. Executable Statements ..
      If (a==zero) Then
        p = abs(b)
      Else If (b==zero) Then
        p = abs(a)
      Else If (abs(a)>=abs(b)) Then
        p = abs(a)*sqrt(1+(b/a)**2)
      Else
        p = abs(b)*sqrt(1+(a/b)**2)
      End If
!
      nagf_blas_dpyth = p
      Return
!
!     End of NAGF_BLAS_DPYTH. ( SPYTH )
!
    End Function
