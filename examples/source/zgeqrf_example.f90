    Program zgeqrf_example

!     ZGEQRF Example Program Text

!     Copyright 2017, Numerical Algorithms Group Ltd. http://www.nag.com

!     .. Use Statements ..
      Use blas_interfaces, Only: dznrm2
      Use lapack_example_aux, Only: nagf_file_print_matrix_complex_gen_comp
      Use lapack_interfaces, Only: zgeqrf, ztrtrs, zunmqr
      Use lapack_precision, Only: dp
!     .. Implicit None Statement ..
      Implicit None
!     .. Parameters ..
      Integer, Parameter :: nb = 64, nin = 5, nout = 6
!     .. Local Scalars ..
      Integer :: i, ifail, info, j, lda, ldb, lwork, m, n, nrhs
!     .. Local Arrays ..
      Complex (Kind=dp), Allocatable :: a(:, :), b(:, :), tau(:), work(:)
      Real (Kind=dp), Allocatable :: rnorm(:)
      Character (1) :: clabs(1), rlabs(1)
!     .. Executable Statements ..
      Write (nout, *) 'ZGEQRF Example Program Results'
      Write (nout, *)
      Flush (nout)
!     Skip heading in data file
      Read (nin, *)
      Read (nin, *) m, n, nrhs
      lda = m
      ldb = m
      lwork = nb*n
      Allocate (a(lda,n), b(ldb,nrhs), tau(n), work(lwork), rnorm(nrhs))

!     Read A and B from data file

      Read (nin, *)(a(i,1:n), i=1, m)
      Read (nin, *)(b(i,1:nrhs), i=1, m)

!     Compute the QR factorization of A
      Call zgeqrf(m, n, a, lda, tau, work, lwork, info)

!     Compute C = (C1) = (Q**H)*B, storing the result in B
!                  (C2)
      Call zunmqr('Left', 'Conjugate transpose', m, nrhs, n, a, lda, tau, b, &
        ldb, work, lwork, info)

!     Compute least squares solutions by back-substitution in
!     R*X = C1
      Call ztrtrs('Upper', 'No transpose', 'Non-Unit', n, nrhs, a, lda, b, &
        ldb, info)

      If (info>0) Then
        Write (nout, *) 'The upper triangular factor, R, of A is singular, '
        Write (nout, *) 'the least squares solution could not be computed'
      Else

!       Print least squares solutions

!       ifail: behaviour on error exit
!              =0 for hard exit, =1 for quiet-soft, =-1 for noisy-soft
        ifail = 0
        Call nagf_file_print_matrix_complex_gen_comp('General', ' ', n, nrhs, &
          b, ldb, 'Bracketed', 'F7.4', 'Least squares solution(s)', 'Integer', &
          rlabs, 'Integer', clabs, 80, 0, ifail)

!       Compute and print estimates of the square roots of the residual
!       sums of squares
        Do j = 1, nrhs
          rnorm(j) = dznrm2(m-n, b(n+1,j), 1)
        End Do

        Write (nout, *)
        Write (nout, *) 'Square root(s) of the residual sum(s) of squares'
        Write (nout, 100) rnorm(1:nrhs)
      End If

100   Format (3X, 1P, 7E11.2)
    End Program
