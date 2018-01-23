    Program dggrqf_example

!     DGGRQF Example Program Text

!     Copyright (c) 2018, Numerical Algorithms Group (NAG Ltd.)
!     For licence see
!       https://github.com/numericalalgorithmsgroup/LAPACK_Examples/blob/master/LICENCE.md

!     .. Use Statements ..
      Use blas_interfaces, Only: dgemv, dnrm2, dtrmv
      Use lapack_interfaces, Only: dggrqf, dormqr, dormrq, dtrtrs
      Use lapack_precision, Only: dp
!     .. Implicit None Statement ..
      Implicit None
!     .. Parameters ..
      Real (Kind=dp), Parameter :: one = 1.0E0_dp
      Integer, Parameter :: nb = 64, nin = 5, nout = 6
!     .. Local Scalars ..
      Real (Kind=dp) :: rnorm
      Integer :: i, info, lda, ldb, lwork, m, n, p
!     .. Local Arrays ..
      Real (Kind=dp), Allocatable :: a(:, :), b(:, :), c(:), d(:), taua(:), &
        taub(:), work(:), x(:)
!     .. Intrinsic Procedures ..
      Intrinsic :: min
!     .. Executable Statements ..
      Write (nout, *) 'DGGRQF Example Program Results'
      Write (nout, *)
!     Skip heading in data file
      Read (nin, *)
      Read (nin, *) m, n, p
      lda = m
      ldb = p
      lwork = nb*(p+n)
      Allocate (a(lda,n), b(ldb,n), c(m), d(p), taua(n), taub(n), work(lwork), &
        x(n))

!     Read B, A, C and D from data file
      Read (nin, *)(a(i,1:n), i=1, m)
      Read (nin, *)(b(i,1:n), i=1, p)
      Read (nin, *) c(1:m)
      Read (nin, *) d(1:p)

!     Compute the generalized RQ factorization of (B,A) as
!     B = (0 T12)*Q,   A = Z*(R11 R12)*Q, where T12, R11
!                            ( 0  R22)
!     are upper triangular
      Call dggrqf(p, m, n, b, ldb, taub, a, lda, taua, work, lwork, info)

!     Set Qx = y. The problem then reduces to:
!                 minimize (Ry - Z^Tc) subject to Ty = d
!     Update c = Z^T*c -> minimize (Ry-c)
      Call dormqr('Left', 'Transpose', m, 1, min(m,n), a, lda, taua, c, m, &
        work, lwork, info)

!     Putting y = (y1), solve T12*w = d for w, storing result in d
!                 (w )
      Call dtrtrs('Upper', 'No transpose', 'Non-unit', p, 1, b(1,n-p+1), ldb, &
        d, p, info)

      If (info>0) Then
        Write (nout, *) 'The upper triangular factor of B is singular, '
        Write (nout, *) 'the least squares solution could not be computed'
        Go To 100
      End If

!     From first n-p rows of (Ry-c) we have: R11*y1 + R12*w = c(1:n-p) = c1
!     Form c1 = c1 - R12*w = R11*y1
      Call dgemv('No transpose', n-p, p, -one, a(1,n-p+1), lda, d, 1, one, c, &
        1)

!     Solve R11*y1 = c1 for y1, storing result in c(1:n-p)
      Call dtrtrs('Upper', 'No transpose', 'Non-unit', n-p, 1, a, lda, c, n-p, &
        info)

      If (info>0) Then
        Write (nout, *) 'The upper triangular factor of A is singular, '
        Write (nout, *) 'the least squares solution could not be computed'
        Go To 100
      End If

!     Copy y into X (first y1, then w)
      x(1:n-p) = c(1:n-p)
      x(n-p+1:n) = d(1:p)

!     Compute x = (Q**T)*y
      Call dormrq('Left', 'Transpose', n, 1, p, b, ldb, taub, x, n, work, &
        lwork, info)

!     The least squares solution is in x, the remainder here is to compute
!     the residual, which equals c2 - R22*w.

!     Upper triangular part of R22 first
      Call dtrmv('Upper', 'No transpose', 'Non-unit', min(m,n)-n+p, &
        a(n-p+1,n-p+1), lda, d, 1)
      Do i = 1, min(m, n) - n + p
        c(n-p+i) = c(n-p+i) - d(i)
      End Do

      If (m<n) Then

!       Additional rectangular part of R22
        Call dgemv('No transpose', m-n+p, n-m, -one, a(n-p+1,m+1), lda, &
          d(m-n+p+1), 1, one, c(n-p+1), 1)
      End If

!     Compute norm of residual sum of squares.
      rnorm = dnrm2(m-(n-p), c(n-p+1), 1)

!     Print least squares solution x
      Write (nout, *) 'Constrained least squares solution'
      Write (nout, 110) x(1:n)

!     Print estimate of the square root of the residual sum of squares
      Write (nout, *)
      Write (nout, *) 'Square root of the residual sum of squares'
      Write (nout, 120) rnorm

100   Continue

110   Format (1X, 7F11.4)
120   Format (3X, 1P, E11.2)
    End Program
