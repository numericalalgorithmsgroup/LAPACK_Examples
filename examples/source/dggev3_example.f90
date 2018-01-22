    Program dggev3_example

!     DGGEV3 Example Program Text

!     Copyright 2017, Numerical Algorithms Group Ltd. http://www.nag.com

!     .. Use Statements ..
      Use lapack_example_aux, Only: nagf_sort_cmplxvec_rank_rearrange, &
        nagf_sort_realmat_rank_rows, nagf_file_print_matrix_complex_gen, &
        nagf_file_print_matrix_real_gen
      Use lapack_interfaces, Only: dggev3
      Use lapack_precision, Only: dp
!     .. Implicit None Statement ..
      Implicit None
!     .. Parameters ..
      Real (Kind=dp), Parameter :: zero = 0.0_dp
      Integer, Parameter :: nin = 5, nout = 6
!     .. Local Scalars ..
      Complex (Kind=dp) :: scal
      Integer :: i, ifail, info, j, k, lda, ldb, ldvr, lwork, n
!     .. Local Arrays ..
      Complex (Kind=dp), Allocatable :: eigval(:), eigvec(:, :)
      Real (Kind=dp), Allocatable :: a(:, :), alphai(:), alphar(:), b(:, :), &
        beta(:), vr(:, :), work(:)
      Real (Kind=dp) :: dummy(1, 1)
      Integer, Allocatable :: irank(:)
!     .. Intrinsic Procedures ..
      Intrinsic :: abs, all, cmplx, conjg, epsilon, maxloc, nint
!     .. Executable Statements ..
      Write (nout, *) 'DGGEV3 Example Program Results'
      Flush (nout)
!     Skip heading in data file
      Read (nin, *)
      Read (nin, *) n
      lda = n
      ldb = n
      ldvr = n
      Allocate (a(lda,n), alphai(n), alphar(n), b(ldb,n), beta(n), vr(ldvr,n), &
        eigvec(n,n), eigval(n), irank(n))

!     Use routine workspace query to get optimal workspace.
      lwork = -1
      Call dggev3('No left vectors', 'Vectors (right)', n, a, lda, b, ldb, &
        alphar, alphai, beta, dummy, 1, vr, ldvr, dummy, lwork, info)

      lwork = nint(dummy(1,1))
      Allocate (work(lwork))

!     Read in the matrices A and B

      Read (nin, *)(a(i,1:n), i=1, n)
      Read (nin, *)(b(i,1:n), i=1, n)

!     Solve the generalized eigenvalue problem

      Call dggev3('No left vectors', 'Vectors (right)', n, a, lda, b, ldb, &
        alphar, alphai, beta, dummy, 1, vr, ldvr, work, lwork, info)
      If (info>0) Then
        Write (nout, *)
        Write (nout, 110) 'Failure in DGGEV3. INFO =', info
        Go To 100
      End If

!     Re-normalize the eigenvectors, largest absolute element real
      j = 0
      Do i = 1, n
        If (alphai(i)==zero) Then
          eigvec(1:n, i) = cmplx(vr(1:n,i), zero, kind=dp)
        Else If (j==0) Then
          eigvec(1:n, i) = cmplx(vr(1:n,i), vr(1:n,i+1), kind=dp)
          j = 1
        Else
          eigvec(1:n, i) = cmplx(vr(1:n,i-1), -vr(1:n,i), kind=dp)
          j = 0
        End If
        work(1:n) = abs(eigvec(1:n,i))
        k = maxloc(work(1:n), 1)
        scal = conjg(eigvec(k,i))/abs(eigvec(k,i))
        eigvec(1:n, i) = eigvec(1:n, i)*scal
      End Do

!     If eigenvalues are finite, order by descending absolute values
      If (all(abs(beta(1:n))>epsilon(1.0E0_dp))) Then
!       add small amount to alphai to distinguish conjugates
        alphai(1:n) = alphai(1:n) + epsilon(1.0E0_dp)*10.0_dp
        eigval(1:n) = cmplx(alphar(1:n), alphai(1:n), kind=dp)
        eigval(1:n) = eigval(1:n)/beta(1:n)
        work(1:n) = abs(eigval(1:n))
        ifail = 0
        Call nagf_sort_realmat_rank_rows(work, n, 1, n, 1, 1, 'Descending', &
          irank, ifail)
        Call nagf_sort_cmplxvec_rank_rearrange(eigval, 1, n, irank, ifail)

!       Print ordered eigenvalues
        ifail = 0
        Call nagf_file_print_matrix_complex_gen('Gen', ' ', 1, n, eigval, 1, &
          'Eigenvalues:', ifail)

!       Order the eigenvectors in the same way and print
        Do j = 1, n
          eigval(1:n) = eigvec(j, 1:n)
          Call nagf_sort_cmplxvec_rank_rearrange(eigval, 1, n, irank, ifail)
          eigvec(j, 1:n) = eigval(1:n)
        End Do

        Write (nout, *)
        Flush (nout)
        ifail = 0
        Call nagf_file_print_matrix_complex_gen('Gen', ' ', n, n, eigvec, n, &
          'Right Eigenvectors (columns):', ifail)
      Else
        Write (nout, *) 'Some of the eigenvalues are infinite'
        Write (nout, *)
        Flush (nout)
        ifail = 0
        Call nagf_file_print_matrix_real_gen('Gen', ' ', 1, n, alphar, 1, &
          'Alpha (real):', ifail)
        Call nagf_file_print_matrix_real_gen('Gen', ' ', 1, n, alphai, 1, &
          'Alpha (imag):', ifail)
        Call nagf_file_print_matrix_real_gen('Gen', ' ', 1, n, beta, 1, &
          'Beta:', ifail)
      End If
100   Continue

110   Format (1X, A, I4)
    End Program
