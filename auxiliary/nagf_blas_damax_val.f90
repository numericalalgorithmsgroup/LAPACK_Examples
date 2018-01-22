    Subroutine nagf_blas_damax_val(n, x, incx, k, r)

!     Mark 22 Release. NAG Copyright 2007.

!     Mat Cross, NAG Oxford

!     K = arg max {ABS(X(J))}, R = max {ABS(X(J))}
!             J                      J

!     for double precision X

!     .. Use Statements ..
      Use lapack_precision, Only: dp
!     .. Implicit None Statement ..
      Implicit None
!     .. Scalar Arguments ..
      Real (Kind=dp), Intent (Out) :: r
      Integer, Intent (In) :: incx, n
      Integer, Intent (Out) :: k
!     .. Array Arguments ..
      Real (Kind=dp), Intent (In) :: x(1+(n-1)*abs(incx))
!     .. Local Scalars ..
      Integer :: i, ix
!     .. Intrinsic Procedures ..
      Intrinsic :: abs
!     .. Executable Statements ..
      Continue

      If (incx/=0) Then

        If (n>0) Then

          If (incx>0) Then
            ix = 1
          Else
            ix = 1 - (n-1)*incx
          End If

          r = abs(x(ix))
          k = 1

          If (n>1) Then

            Do i = 2, n
              ix = ix + incx

              If (r<abs(x(ix))) Then
                r = abs(x(ix))
                k = i
              End If

            End Do

          End If

        Else
          r = 0.0E0_dp
          k = 0
        End If

      Else
        r = 0.0E0_dp
        k = 0

      End If

      Return

!     End of NAGF_BLAS_DAMAX_VAL.

    End Subroutine
