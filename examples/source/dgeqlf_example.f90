    Program dgeqlf_example

!     DGEQLF Example Program Text

!     Copyright (c) 2018, Numerical Algorithms Group (NAG Ltd.)
!     For licence see
!       https://github.com/numericalalgorithmsgroup/LAPACK_Examples/blob/master/LICENCE.md

!     .. Use Statements ..
      Use blas_interfaces, Only: dnrm2
      Use lapack_example_aux, Only: nagf_file_print_matrix_real_gen
      Use lapack_interfaces, Only: dgeqlf, dormql, dtrtrs
      Use lapack_precision, Only: dp
!     .. Implicit None Statement ..
      Implicit None
!     .. Parameters ..
      Integer, Parameter :: nb = 64, nin = 5, nout = 6
!     .. Local Scalars ..
      Integer :: i, ifail, info, j, lda, ldb, lwork, m, n, nrhs
!     .. Local Arrays ..
      Real (Kind=dp), Allocatable :: a(:, :), b(:, :), rnorm(:), tau(:), &
        work(:)
!     .. Executable Statements ..
      Write (nout, *) 'DGEQLF Example Program Results'
      Write (nout, *)
      Flush (nout)
!     Skip heading in data file
      Read (nin, *)
      Read (nin, *) m, n, nrhs
      lda = m
      ldb = m
      lwork = nb*n
      Allocate (a(lda,n), b(ldb,nrhs), rnorm(nrhs), tau(n), work(lwork))

!     Read A and B from data file

      Read (nin, *)(a(i,1:n), i=1, m)
      Read (nin, *)(b(i,1:nrhs), i=1, m)

!     Compute the QL factorization of A
      Call dgeqlf(m, n, a, lda, tau, work, lwork, info)

!     Compute C = (C1) = (Q**T)*B, storing the result in B
!                  (C2)
      Call dormql('Left', 'Transpose', m, nrhs, n, a, lda, tau, b, ldb, work, &
        lwork, info)

!     Compute least squares solutions by back-substitution in
!     L*X = C2
      Call dtrtrs('Lower', 'No transpose', 'Non-Unit', n, nrhs, a(m-n+1,1), &
        lda, b(m-n+1,1), ldb, info)

      If (info>0) Then
        Write (nout, *) 'The lower triangular factor, L, of A is singular, '
        Write (nout, *) 'the least squares solution could not be computed'
      Else

!       Print least squares solution(s)

!       ifail: behaviour on error exit
!              =0 for hard exit, =1 for quiet-soft, =-1 for noisy-soft
        ifail = 0
        Call nagf_file_print_matrix_real_gen('General', ' ', n, nrhs, &
          b(m-n+1,1), ldb, 'Least squares solution(s)', ifail)

!       Compute and print estimates of the square roots of the residual
!       sums of squares

        Do j = 1, nrhs
          rnorm(j) = dnrm2(m-n, b(1,j), 1)
        End Do

        Write (nout, *)
        Write (nout, *) 'Square root(s) of the residual sum(s) of squares'
        Write (nout, 100) rnorm(1:nrhs)
      End If

100   Format (5X, 1P, 7E11.2)
    End Program
