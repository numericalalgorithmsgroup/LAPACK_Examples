    Program dgerqf_example

!     DGERQF Example Program Text

!     Copyright (c) 2018, Numerical Algorithms Group (NAG Ltd.)
!     For licence see
!       https://github.com/numericalalgorithmsgroup/LAPACK_Examples/blob/master/LICENCE.md

!     .. Use Statements ..
      Use lapack_interfaces, Only: dgerqf, dormrq, dtrtrs
      Use lapack_precision, Only: dp
!     .. Implicit None Statement ..
      Implicit None
!     .. Parameters ..
      Real (Kind=dp), Parameter :: zero = 0.0E0_dp
      Integer, Parameter :: nb = 64, nin = 5, nout = 6
!     .. Local Scalars ..
      Integer :: i, info, lda, lwork, m, n
!     .. Local Arrays ..
      Real (Kind=dp), Allocatable :: a(:, :), b(:), tau(:), work(:), x(:)
!     .. Executable Statements ..
      Write (nout, *) 'DGERQF Example Program Results'
      Write (nout, *)
!     Skip heading in data file
      Read (nin, *)
      Read (nin, *) m, n
      lda = m
      lwork = nb*m
      Allocate (a(lda,n), b(m), tau(m), work(lwork), x(n))

!     Read the matrix A and the vector b from data file

      Read (nin, *)(a(i,1:n), i=1, m)
      Read (nin, *) b(1:m)

!     Compute the RQ factorization of A
      Call dgerqf(m, n, a, lda, tau, work, lwork, info)

!     Copy the m element vector b into elements x(n-m+1), ..., x(n) of x

      x(n-m+1:n) = b(1:m)

!     Solve R*y2 = b, storing the result in x2
      Call dtrtrs('Upper', 'No transpose', 'Non-Unit', m, 1, a(1,n-m+1), lda, &
        x(n-m+1), m, info)

      If (info>0) Then
        Write (nout, *) 'The upper triangular factor, R, of A is singular, '
        Write (nout, *) 'the least squares solution could not be computed'
      Else

        x(1:n-m) = zero

!       Compute the minimum-norm solution x = (Q**T)*y
        Call dormrq('Left', 'Transpose', n, 1, m, a, lda, tau, x, n, work, &
          lwork, info)

!       Print minimum-norm solution

        Write (nout, *) 'Minimum-norm solution'
        Write (nout, 100) x(1:n)

      End If

100   Format (1X, 8F9.4)
    End Program
