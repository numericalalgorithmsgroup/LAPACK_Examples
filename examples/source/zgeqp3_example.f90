    Program zgeqp3_example

!     ZGEQP3 Example Program Text

!     Copyright (c) 2018, Numerical Algorithms Group (NAG Ltd.)
!     For licence see
!       https://github.com/numericalalgorithmsgroup/LAPACK_Examples/blob/master/LICENCE.md

!     .. Use Statements ..
      Use blas_interfaces, Only: dznrm2, ztrsm
      Use lapack_example_aux, Only: nagf_file_print_matrix_complex_gen_comp
      Use lapack_interfaces, Only: zgeqp3, zunmqr
      Use lapack_precision, Only: dp
!     .. Implicit None Statement ..
      Implicit None
!     .. Parameters ..
      Complex (Kind=dp), Parameter :: one = (1.0E0_dp, 0.0E0_dp)
      Complex (Kind=dp), Parameter :: zero = (0.0E0_dp, 0.0E0_dp)
      Integer, Parameter :: inc1 = 1, nb = 64, nin = 5, nout = 6
!     .. Local Scalars ..
      Real (Kind=dp) :: tol
      Integer :: i, ifail, info, j, k, lda, ldb, lwork, m, n, nrhs
!     .. Local Arrays ..
      Complex (Kind=dp), Allocatable :: a(:, :), b(:, :), tau(:), work(:)
      Real (Kind=dp), Allocatable :: rnorm(:), rwork(:)
      Integer, Allocatable :: jpvt(:)
      Character (1) :: clabs(1), rlabs(1)
!     .. Intrinsic Procedures ..
      Intrinsic :: abs
!     .. Executable Statements ..
      Write (nout, *) 'ZGEQP3 Example Program Results'
      Write (nout, *)
!     Skip heading in data file
      Read (nin, *)
      Read (nin, *) m, n, nrhs
      lda = m
      ldb = m
      lwork = (n+1)*nb
      Allocate (a(lda,n), b(ldb,nrhs), tau(n), work(lwork), rnorm(nrhs), &
        rwork(2*n), jpvt(n))

!     Read A and B from data file

      Read (nin, *)(a(i,1:n), i=1, m)
      Read (nin, *)(b(i,1:nrhs), i=1, m)

!     Initialize JPVT to be zero so that all columns are free

      jpvt(1:n) = 0

!     Compute the QR factorization of A

      Call zgeqp3(m, n, a, lda, jpvt, tau, work, lwork, rwork, info)

!     Compute C = (C1) = (Q**H)*B, storing the result in B
!                  (C2)

      Call zunmqr('Left', 'Conjugate Transpose', m, nrhs, n, a, lda, tau, b, &
        ldb, work, lwork, info)

!     Choose TOL to reflect the relative accuracy of the input data

      tol = 0.01_dp

!     Determine and print the rank, K, of R relative to TOL

loop: Do k = 1, n
        If (abs(a(k,k))<=tol*abs(a(1,1))) Then
          Exit loop
        End If
      End Do loop
      k = k - 1

      Write (nout, *) 'Tolerance used to estimate the rank of A'
      Write (nout, 100) tol
      Write (nout, *) 'Estimated rank of A'
      Write (nout, 110) k
      Write (nout, *)
      Flush (nout)

!     Compute least squares solutions by back-substitution in
!     R(1:K,1:K)*Y = C1, storing the result in B

      Call ztrsm('Left', 'Upper', 'No transpose', 'Non-Unit', k, nrhs, one, a, &
        lda, b, ldb)

!     Compute estimates of the square roots of the residual sums of
!     squares (2-norm of each of the columns of C2)

      Do j = 1, nrhs
        rnorm(j) = dznrm2(m-k, b(k+1,j), inc1)
      End Do

!     Set the remaining elements of the solutions to zero (to give
!     the basic solutions)

      b(k+1:n, 1:nrhs) = zero

!     Permute the least squares solutions stored in B to give X = P*Y

      Do j = 1, nrhs
        work(jpvt(1:n)) = b(1:n, j)
        b(1:n, j) = work(1:n)
      End Do

!     Print least squares solutions

!     ifail: behaviour on error exit
!             =0 for hard exit, =1 for quiet-soft, =-1 for noisy-soft
      ifail = 0
      Call nagf_file_print_matrix_complex_gen_comp('General', ' ', n, nrhs, b, &
        ldb, 'Bracketed', 'F7.4', 'Least squares solution(s)', 'Integer', &
        rlabs, 'Integer', clabs, 80, 0, ifail)

!     Print the square roots of the residual sums of squares

      Write (nout, *)
      Write (nout, *) 'Square root(s) of the residual sum(s) of squares'
      Write (nout, 100) rnorm(1:nrhs)

100   Format (3X, 1P, 7E11.2)
110   Format (1X, I8)
    End Program
