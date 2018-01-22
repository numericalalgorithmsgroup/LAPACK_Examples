    Subroutine nagf_blas_zddscl(n, d, incd, x, incx)
!     Mark 12 Release. NAG Copyright 1986.

!     NAGF_BLAS_ZDDSCL performs the operation

!     x := diag( d )*x

!     Nag Fortran 77 O( n ) basic linear algebra routine.

!     -- Written on 12-February-1986.
!     Sven Hammarling, Nag Central Office.

!     .. Use Statements ..
      Use blas_interfaces, Only: zdscal
      Use lapack_precision, Only: dp
!     .. Implicit None Statement ..
      Implicit None
!     .. Scalar Arguments ..
      Integer, Intent (In) :: incd, incx, n
!     .. Array Arguments ..
      Complex (Kind=dp), Intent (Inout) :: x(*)
      Real (Kind=dp), Intent (In) :: d(*)
!     .. Local Scalars ..
      Integer :: i, id, ix
!     .. Intrinsic Procedures ..
      Intrinsic :: abs, cmplx
!     .. Executable Statements ..
      Continue

      If (n>0) Then
        If ((incd==0) .And. (incx/=0)) Then
          Call zdscal(n=n, alpha=d(1), x=x, incx=abs(incx))
        Else If ((incd==incx) .And. (incd>0)) Then
          Do id = 1, 1 + (n-1)*incd, incd
            x(id) = cmplx(d(id), kind=dp)*x(id)
          End Do
        Else
          If (incx>=0) Then
            ix = 1
          Else
            ix = 1 - (n-1)*incx
          End If
          If (incd>0) Then
            Do id = 1, 1 + (n-1)*incd, incd
              x(ix) = cmplx(d(id), kind=dp)*x(ix)
              ix = ix + incx
            End Do
          Else
            id = 1 - (n-1)*incd
            Do i = 1, n
              x(ix) = cmplx(d(id), kind=dp)*x(ix)
              id = id + incd
              ix = ix + incx
            End Do
          End If
        End If
      End If

      Return

!     End of NAGF_BLAS_ZDDSCL.

    End Subroutine
