    Program ztpqrt_example

!     ZTPQRT Example Program Text

!     Copyright 2017, Numerical Algorithms Group Ltd. http://www.nag.com

!     .. Use Statements ..
      Use blas_interfaces, Only: dznrm2
      Use lapack_example_aux, Only: nagf_file_print_matrix_complex_gen_comp
      Use lapack_interfaces, Only: zgemqrt, zgeqrt, ztpmqrt, ztpqrt, ztrtrs
      Use lapack_precision, Only: dp
!     .. Implicit None Statement ..
      Implicit None
!     .. Parameters ..
      Integer, Parameter :: nbmax = 64, nin = 5, nout = 6
!     .. Local Scalars ..
      Integer :: i, ifail, info, j, lda, ldb, ldt, lwork, m, n, nb, nrhs
!     .. Local Arrays ..
      Complex (Kind=dp), Allocatable :: a(:, :), b(:, :), c(:, :), t(:, :), &
        work(:)
      Real (Kind=dp), Allocatable :: rnorm(:)
      Character (1) :: clabs(1), rlabs(1)
!     .. Intrinsic Procedures ..
      Intrinsic :: max, min
!     .. Executable Statements ..
      Write (nout, *) 'ZTPQRT Example Program Results'
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
      Allocate (a(lda,n), b(ldb,nrhs), c(ldb,nrhs), rnorm(nrhs), t(ldt,min(m, &
        n)), work(lwork))

!     Read A and B from data file

      Read (nin, *)(a(i,1:n), i=1, m)
      Read (nin, *)(b(i,1:nrhs), i=1, m)

      c(1:m, 1:nrhs) = b(1:m, 1:nrhs)
!     Compute the QR factorization of first n rows of A
      Call zgeqrt(n, n, nb, a, lda, t, ldt, work, info)

!     Compute C = (C1) = (Q**H)*B, storing the result in C
!                 (C2)
      Call zgemqrt('Left', 'Conjugate Transpose', n, nrhs, n, nb, a, lda, t, &
        ldt, c, ldb, work, info)

      b(1:n, 1:nrhs) = c(1:n, 1:nrhs)
!     Compute least squares solutions for first n rows by back-substitution in
!     R*X = C1
      Call ztrtrs('Upper', 'No transpose', 'Non-Unit', n, nrhs, a, lda, c, &
        ldb, info)

      If (info>0) Then
        Write (nout, *) 'The upper triangular factor, R, of A is singular, '
        Write (nout, *) 'the least squares solution could not be computed'
      Else

!       Print solution using first n rows

!       ifail: behaviour on error exit
!              =0 for hard exit, =1 for quiet-soft, =-1 for noisy-soft
        ifail = 0
        Call nagf_file_print_matrix_complex_gen_comp('General', ' ', n, nrhs, &
          c, ldb, 'Bracketed', 'F7.4', 'solution(s) for n rows', 'Integer', &
          rlabs, 'Integer', clabs, 80, 0, ifail)

      End If

!     Now add the remaining rows and perform QR update
      Call ztpqrt(m-n, n, 0, nb, a, lda, a(n+1,1), lda, t, ldt, work, info)

!     Apply orthogonal transformations to C
      Call ztpmqrt('Left', 'Conjugate Transpose', m-n, nrhs, n, 0, nb, &
        a(n+1,1), lda, t, ldt, b, ldb, b(5,1), ldb, work, info)
!     Compute least squares solutions for first n rows by back-substitution in
!     R*X = C1
      Call ztrtrs('Upper', 'No transpose', 'Non-Unit', n, nrhs, a, lda, b, &
        ldb, info)

      If (info>0) Then
        Write (nout, *) 'The upper triangular factor, R, of A is singular, '
        Write (nout, *) 'the least squares solution could not be computed'
      Else

!       Print least squares solutions
        Write (nout, *)
        Flush (nout)
        ifail = 0
        Call nagf_file_print_matrix_complex_gen_comp('G', ' ', n, nrhs, b, &
          ldb, 'Bracketed', 'F7.4', 'Least squares solution(s) for all rows', &
          'Integer', rlabs, 'Integer', clabs, 80, 0, ifail)

!       Compute and print estimates of the square roots of the residual
!       sums of squares

        Do j = 1, nrhs
          rnorm(j) = dznrm2(m-n, b(n+1,j), 1)
        End Do

        Write (nout, *)
        Write (nout, *) 'Square root(s) of the residual sum(s) of squares'
        Write (nout, 100) rnorm(1:nrhs)
      End If

100   Format (5X, 1P, 7E11.2)
    End Program
