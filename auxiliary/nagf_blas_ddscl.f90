    Subroutine nagf_blas_ddscl(n, d, incd, x, incx)
!     Mark 12 Release. NAG Copyright 1986.

!     NAGF_BLAS_DDSCL performs the operation

!     x := diag( d )*x

!     Nag Fortran 77 O( n ) basic linear algebra routine.

!     -- Written on 22-September-1983.
!     Sven Hammarling, Nag Central Office.

!     .. Use Statements ..
      Use blas_interfaces, Only: dscal
      Use lapack_precision, Only: dp
!     .. Implicit None Statement ..
      Implicit None
!     .. Scalar Arguments ..
      Integer, Intent (In) :: incd, incx, n
!     .. Array Arguments ..
      Real (Kind=dp), Intent (In) :: d(*)
      Real (Kind=dp), Intent (Inout) :: x(*)
!     .. Local Scalars ..
      Integer :: i, id, ix
!     .. Intrinsic Procedures ..
      Intrinsic :: abs
!     .. Executable Statements ..
      Continue

      If (n>0) Then
        If ((incd==0) .And. (incx/=0)) Then
          Call dscal(n=n, alpha=d(1), x=x, incx=abs(incx))
        Else If ((incd==incx) .And. (incd>0)) Then
          Do id = 1, 1 + (n-1)*incd, incd
            x(id) = d(id)*x(id)
          End Do
        Else
          If (incx>=0) Then
            ix = 1
          Else
            ix = 1 - (n-1)*incx
          End If
          If (incd>0) Then
            Do id = 1, 1 + (n-1)*incd, incd
              x(ix) = d(id)*x(ix)
              ix = ix + incx
            End Do
          Else
            id = 1 - (n-1)*incd
            Do i = 1, n
              x(ix) = d(id)*x(ix)
              id = id + incd
              ix = ix + incx
            End Do
          End If
        End If
      End If

      Return

!     End of NAGF_BLAS_DDSCL.

    End Subroutine
