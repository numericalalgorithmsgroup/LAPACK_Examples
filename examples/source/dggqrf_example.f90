    Program dggqrf_example

!     DGGQRF Example Program Text

!     Copyright 2017, Numerical Algorithms Group Ltd. http://www.nag.com

!     .. Use Statements ..
      Use blas_interfaces, Only: dgemv, dnrm2
      Use lapack_interfaces, Only: dggqrf, dormqr, dormrq, dtrtrs
      Use lapack_precision, Only: dp
!     .. Implicit None Statement ..
      Implicit None
!     .. Parameters ..
      Real (Kind=dp), Parameter :: one = 1.0E0_dp
      Real (Kind=dp), Parameter :: zero = 0.0E0_dp
      Integer, Parameter :: nb = 64, nin = 5, nout = 6
!     .. Local Scalars ..
      Real (Kind=dp) :: rnorm
      Integer :: i, info, lda, ldb, lwork, m, n, p
!     .. Local Arrays ..
      Real (Kind=dp), Allocatable :: a(:, :), b(:, :), d(:), taua(:), taub(:), &
        work(:), y(:)
!     .. Intrinsic Procedures ..
      Intrinsic :: max, min
!     .. Executable Statements ..
      Write (nout, *) 'DGGQRF Example Program Results'
      Write (nout, *)
!     Skip heading in data file
      Read (nin, *)
      Read (nin, *) n, m, p
      lda = n
      ldb = n
      lwork = nb*(m+p)
      Allocate (a(lda,m), b(ldb,p), d(n), taua(m), taub(m+p), work(lwork), &
        y(p))

!     Read A, B and D from data file
      Read (nin, *)(a(i,1:m), i=1, n)
      Read (nin, *)(b(i,1:p), i=1, n)
      Read (nin, *) d(1:n)

!     Compute the generalized QR factorization of (A,B) as
!     A = Q*(R),   B = Q*(T11 T12)*Z
!            (0)          ( 0  T22)
      Call dggqrf(n, m, p, a, lda, taua, b, ldb, taub, work, lwork, info)

!     Compute c = (c1) = (Q**T)*d, storing the result in D
!                  (c2)
      Call dormqr('Left', 'Transpose', n, 1, m, a, lda, taua, d, n, work, &
        lwork, info)

!     Putting Z*y = w = (w1), set w1 = 0, storing the result in Y1
!                        (w2)
      y(1:m+p-n) = zero

      If (n>m) Then

!       Copy c2 into Y2
        y(m+p-n+1:p) = d(m+1:n)

!       Solve T22*w2 = c2 for w2, storing result in Y2
        Call dtrtrs('Upper', 'No transpose', 'Non-unit', n-m, 1, &
          b(m+1,m+p-n+1), ldb, y(m+p-n+1), n-m, info)

        If (info>0) Then
          Write (nout, *) &
            'The upper triangular factor, T22, of B is singular, '
          Write (nout, *) 'the least squares solution could not be computed'
          Go To 100
        End If

!       Compute estimate of the square root of the residual sum of squares
!       norm(y) = norm(w2)

        rnorm = dnrm2(n-m, y(m+p-n+1), 1)

!       Form c1 - T12*w2 in D
        Call dgemv('No transpose', m, n-m, -one, b(1,m+p-n+1), ldb, &
          y(m+p-n+1), 1, one, d, 1)
      End If

!     Solve R*x = c1 - T12*w2 for x
      Call dtrtrs('Upper', 'No transpose', 'Non-unit', m, 1, a, lda, d, m, &
        info)

      If (info>0) Then
        Write (nout, *) 'The upper triangular factor, R, of A is singular, '
        Write (nout, *) 'the least squares solution could not be computed'
      Else

!       Compute y = (Z**T)*w
        Call dormrq('Left', 'Transpose', p, 1, min(n,p), b(max(1, &
          n-p+1),1), ldb, taub, y, p, work, lwork, info)

!       Print least squares solution x

        Write (nout, *) 'Generalized least squares solution'
        Write (nout, 110) d(1:m)

!       Print residual vector y

        Write (nout, *)
        Write (nout, *) 'Residual vector'
        Write (nout, 120) y(1:p)

!       Print estimate of the square root of the residual sum of squares

        Write (nout, *)
        Write (nout, *) 'Square root of the residual sum of squares'
        Write (nout, 120) rnorm
      End If
100   Continue

110   Format (1X, 7F11.4)
120   Format (3X, 1P, 7E11.2)
    End Program
