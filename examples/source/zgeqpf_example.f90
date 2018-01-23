    Program zgeqpf_example

!     ZGEQPF Example Program Text

!     Copyright (c) 2018, Numerical Algorithms Group (NAG Ltd.)
!     For licence see
!       https://github.com/numericalalgorithmsgroup/LAPACK_Examples/blob/master/LICENCE.md

!     .. Use Statements ..
      Use blas_interfaces, Only: ztrsv
      Use lapack_example_aux, Only: nagf_file_print_matrix_complex_gen_comp
      Use lapack_interfaces, Only: zgeqp3, zunmqr
      Use lapack_precision, Only: dp
!     .. Implicit None Statement ..
      Implicit None
!     .. Parameters ..
      Complex (Kind=dp), Parameter :: zero = (0.0E0_dp, 0.0E0_dp)
      Integer, Parameter :: nin = 5, nout = 6
!     .. Local Scalars ..
      Real (Kind=dp) :: tol
      Integer :: i, ifail, info, k, lda, ldb, ldx, lwork, m, n, nrhs
!     .. Local Arrays ..
      Complex (Kind=dp), Allocatable :: a(:, :), b(:, :), tau(:), work(:), &
        x(:, :)
      Real (Kind=dp), Allocatable :: rwork(:)
      Integer, Allocatable :: jpvt(:)
      Character (1) :: clabs(1), rlabs(1)
!     .. Intrinsic Procedures ..
      Intrinsic :: abs
!     .. Executable Statements ..
      Write (nout, *) 'ZGEQPF Example Program Results'
!     Skip heading in data file
      Read (nin, *)
      Read (nin, *) m, n, nrhs
      lda = m
      ldb = m
      ldx = m
      lwork = 64*n
      Allocate (a(lda,n), b(ldb,nrhs), tau(n), work(lwork), x(ldx,nrhs), &
        rwork(2*n), jpvt(n))

!     Read A and B from data file

      Read (nin, *)(a(i,1:n), i=1, m)
      Read (nin, *)(b(i,1:nrhs), i=1, m)

!     Initialize JPVT to be zero so that all columns are free

      jpvt(1:n) = 0

!     Compute the QR factorization of A
      Call zgeqp3(m, n, a, lda, jpvt, tau, work, lwork, rwork, info)

!     Choose TOL to reflect the relative accuracy of the input data

      tol = 0.01_dp

!     Determine which columns of R to use

loop: Do k = 1, n
        If (abs(a(k,k))<=tol*abs(a(1,1))) Then
          Exit loop
        End If
      End Do loop

!     Compute C = (Q**H)*B, storing the result in B

      k = k - 1

      Call zunmqr('Left', 'Conjugate Transpose', m, nrhs, k, a, lda, tau, b, &
        ldb, work, lwork, info)

!     Compute least squares solution by back-substitution in R*B = C

      Do i = 1, nrhs

        Call ztrsv('Upper', 'No transpose', 'Non-Unit', k, a, lda, b(1,i), 1)

!       Set the unused elements of the I-th solution vector to zero

        b(k+1:n, i) = zero

      End Do

!     Unscramble the least squares solution stored in B

      Do i = 1, n
        x(jpvt(i), 1:nrhs) = b(i, 1:nrhs)
      End Do

!     Print least squares solution

      Write (nout, *)
      Flush (nout)

!     ifail: behaviour on error exit
!             =0 for hard exit, =1 for quiet-soft, =-1 for noisy-soft
      ifail = 0
      Call nagf_file_print_matrix_complex_gen_comp('General', ' ', n, nrhs, x, &
        ldx, 'Bracketed', 'F7.4', 'Least squares solution', 'Integer', rlabs, &
        'Integer', clabs, 80, 0, ifail)

    End Program
