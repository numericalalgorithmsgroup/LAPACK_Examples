    Program dgeqrt_example

!     DGEQRT Example Program Text

!     Copyright (c) 2018, Numerical Algorithms Group (NAG Ltd.)
!     For licence see
!       https://github.com/numericalalgorithmsgroup/LAPACK_Examples/blob/master/LICENCE.md

!     .. Use Statements ..
      Use blas_interfaces, Only: dnrm2
      Use lapack_example_aux, Only: nagf_file_print_matrix_real_gen
      Use lapack_interfaces, Only: dgemqrt, dgeqrt, dtrtrs
      Use lapack_precision, Only: dp
!     .. Implicit None Statement ..
      Implicit None
!     .. Parameters ..
      Integer, Parameter :: nbmax = 64, nin = 5, nout = 6
!     .. Local Scalars ..
      Integer :: i, ifail, info, j, lda, ldb, ldt, lwork, m, n, nb, nrhs
!     .. Local Arrays ..
      Real (Kind=dp), Allocatable :: a(:, :), b(:, :), rnorm(:), t(:, :), &
        work(:)
!     .. Intrinsic Procedures ..
      Intrinsic :: max, min
!     .. Executable Statements ..
      Write (nout, *) 'DGEQRT Example Program Results'
      Write (nout, *)
      Flush (nout)
!     Skip heading in data file
      Read (nin, *)
      Read (nin, *) m, n, nrhs
      lda = m
      ldb = m
      nb = min(m, n, nbmax)
      ldt = nb
      lwork = nb*max(n, m)
      Allocate (a(lda,n), b(ldb,nrhs), rnorm(nrhs), t(ldt,min(m, &
        n)), work(lwork))

!     Read A and B from data file

      Read (nin, *)(a(i,1:n), i=1, m)
      Read (nin, *)(b(i,1:nrhs), i=1, m)

!     Compute the QR factorization of A
      Call dgeqrt(m, n, nb, a, lda, t, ldt, work, info)

!     Compute C = (C1) = (Q**T)*B, storing the result in B
!                 (C2)
      Call dgemqrt('Left', 'Transpose', m, nrhs, n, nb, a, lda, t, ldt, b, &
        ldb, work, info)

!     Compute least squares solutions by back-substitution in
!     R*X = C1
      Call dtrtrs('Upper', 'No transpose', 'Non-Unit', n, nrhs, a, lda, b, &
        ldb, info)

      If (info>0) Then
        Write (nout, *) 'The upper triangular factor, R, of A is singular, '
        Write (nout, *) 'the least squares solution could not be computed'
      Else

!       Print least squares solutions

!       ifail: behaviour on error exit
!              =0 for hard exit, =1 for quiet-soft, =-1 for noisy-soft
        ifail = 0
        Call nagf_file_print_matrix_real_gen('General', ' ', n, nrhs, b, ldb, &
          'Least squares solution(s)', ifail)

!       Compute and print estimates of the square roots of the residual
!       sums of squares

        Do j = 1, nrhs
          rnorm(j) = dnrm2(m-n, b(n+1,j), 1)
        End Do

        Write (nout, *)
        Write (nout, *) 'Square root(s) of the residual sum(s) of squares'
        Write (nout, 100) rnorm(1:nrhs)
      End If

100   Format (5X, 1P, 7E11.2)
    End Program
