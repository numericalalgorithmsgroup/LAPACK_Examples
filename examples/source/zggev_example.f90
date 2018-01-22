    Program zggev_example

!     ZGGEV Example Program Text

!     Copyright 2017, Numerical Algorithms Group Ltd. http://www.nag.com

!     .. Use Statements ..
      Use lapack_example_aux, Only: nagf_file_print_matrix_complex_gen, &
        nagf_sort_cmplxvec_rank_rearrange, nagf_sort_realvec_rank
      Use lapack_interfaces, Only: zggev
      Use lapack_precision, Only: dp
!     .. Implicit None Statement ..
      Implicit None
!     .. Parameters ..
      Real (Kind=dp), Parameter :: one = 1.0_dp
      Real (Kind=dp), Parameter :: zero = 0.0_dp
      Integer, Parameter :: nb = 64, nin = 5, nout = 6
      Complex (Kind=dp), Parameter :: cone = (one, zero)
!     .. Local Scalars ..
      Complex (Kind=dp) :: scal
      Integer :: i, ifail, info, j, k, lda, ldb, ldvr, lwork, n
!     .. Local Arrays ..
      Complex (Kind=dp), Allocatable :: a(:, :), alpha(:), b(:, :), beta(:), &
        vr(:, :), work(:)
      Complex (Kind=dp) :: dummy(1, 1)
      Real (Kind=dp), Allocatable :: rwork(:)
      Integer, Allocatable :: irank(:)
!     .. Intrinsic Procedures ..
      Intrinsic :: abs, all, epsilon, max, maxloc, nint, real
!     .. Executable Statements ..
      Write (nout, *) 'ZGGEV Example Program Results'
      Flush (nout)
!     Skip heading in data file
      Read (nin, *)
      Read (nin, *) n
      lda = n
      ldb = n
      ldvr = n
      Allocate (a(lda,n), alpha(n), b(ldb,n), beta(n), vr(ldvr,n), rwork(8*n))

!     Use routine workspace query to get optimal workspace.
      lwork = -1
      Call zggev('No left vectors', 'Vectors (right)', n, a, lda, b, ldb, &
        alpha, beta, dummy, 1, vr, ldvr, dummy, lwork, rwork, info)

!     Make sure that there is enough workspace for block size nb.
      lwork = max((nb+1)*n, nint(real(dummy(1,1))))
      Allocate (work(lwork))

!     Read in the matrices A and B

      Read (nin, *)(a(i,1:n), i=1, n)
      Read (nin, *)(b(i,1:n), i=1, n)

!     Solve the generalized eigenvalue problem

      Call zggev('No left vectors', 'Vectors (right)', n, a, lda, b, ldb, &
        alpha, beta, dummy, 1, vr, ldvr, work, lwork, rwork, info)

      If (info>0) Then
        Write (nout, *)
        Write (nout, 100) 'Failure in ZGGEV. INFO =', info
      Else
!       Re-normalize the eigenvectors, largest absolute element real (=1)
        Do i = 1, n
          rwork(1:n) = abs(vr(1:n,i))
          k = maxloc(rwork(1:n), 1)
          scal = cone/vr(k, i)
          vr(1:n, i) = vr(1:n, i)*scal
          vr(k, i) = cone
        End Do

        Write (nout, *)
        Flush (nout)
        If (all(abs(beta(1:n))>epsilon(1.0E0_dp))) Then
!         Reorder eigenvalues by descending absolute value and print
          alpha(1:n) = alpha(1:n)/beta(1:n)
          rwork(1:n) = abs(alpha(1:n))
          Allocate (irank(n))
          ifail = 0
          Call nagf_sort_realvec_rank(rwork, 1, n, 'Descending', irank, ifail)
          Call nagf_sort_cmplxvec_rank_rearrange(alpha, 1, n, irank, ifail)
          ifail = 0
          Call nagf_file_print_matrix_complex_gen('Gen', ' ', 1, n, alpha, 1, &
            'Eigenvalues:', ifail)

!         Reorder eigenvectors accordingly
          Do j = 1, n
            beta(1:n) = vr(j, 1:n)
            Call nagf_sort_cmplxvec_rank_rearrange(beta, 1, n, irank, ifail)
            vr(j, 1:n) = beta(1:n)
          End Do
        Else
          Write (nout, *) &
            'Some of the eigenvalues are infinite or undetermined'
          Write (nout, *)
          Flush (nout)
          ifail = 0
          Call nagf_file_print_matrix_complex_gen('Gen', ' ', 1, n, alpha, 1, &
            'Alpha:', ifail)
          Call nagf_file_print_matrix_complex_gen('Gen', ' ', 1, n, beta, 1, &
            'Beta:', ifail)
        End If
        Write (nout, *)
        Flush (nout)
        ifail = 0
        Call nagf_file_print_matrix_complex_gen('Gen', ' ', n, n, vr, ldvr, &
          'Eigenvectors (columns):', ifail)
      End If

100   Format (1X, A, I4)
    End Program
