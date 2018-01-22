!   ZGGESX Example Program Text
!   Copyright 2017, Numerical Algorithms Group Ltd. http://www.nag.com

    Module zggesx_example_mod

!     ZGGESX Example Program Module:
!            Parameters and User-defined Routines

!     .. Use Statements ..
      Use lapack_precision, Only: dp
!     .. Implicit None Statement ..
      Implicit None
!     .. Accessibility Statements ..
      Private
      Public :: selctg
!     .. Parameters ..
      Integer, Parameter, Public :: nb = 64, nin = 5, nout = 6
      Logical, Parameter, Public :: chkfac = .False., prcond = .False., &
        prmat = .False.
    Contains
      Function selctg(a, b)

!       Logical function selctg for use with ZGGESX (ZGGESX)
!       Returns the value .TRUE. if the absolute value of the eigenvalue
!       a/b < 6.0

!       .. Function Return Value ..
        Logical :: selctg
!       .. Scalar Arguments ..
        Complex (Kind=dp), Intent (In) :: a, b
!       .. Intrinsic Procedures ..
        Intrinsic :: abs
!       .. Executable Statements ..
        selctg = (abs(a)<6.0_dp*abs(b))
        Return
      End Function
    End Module
    Program zggesx_example

!     ZGGESX Example Main Program

!     .. Use Statements ..
      Use blas_interfaces, Only: zgemm
      Use zggesx_example_mod, Only: chkfac, nb, nin, nout, prcond, prmat, selctg
      Use lapack_example_aux, Only: nagf_sort_realvec_rank, nagf_blas_dpyth, &
        nagf_file_print_matrix_complex_gen_comp, &
        nagf_sort_cmplxvec_rank_rearrange
      Use lapack_interfaces, Only: zggesx, zlange
      Use lapack_precision, Only: dp
!     .. Implicit None Statement ..
      Implicit None
!     .. Local Scalars ..
      Complex (Kind=dp) :: alph, bet
      Real (Kind=dp) :: abnorm, anorm, bnorm, eps, normd, norme, tol
      Integer :: i, ifail, info, lda, ldb, ldc, ldd, lde, ldvsl, ldvsr, &
        liwork, lwork, n, sdim
      Logical :: factor
!     .. Local Arrays ..
      Complex (Kind=dp), Allocatable :: a(:, :), alpha(:), b(:, :), beta(:), &
        c(:, :), d(:, :), e(:, :), vsl(:, :), vsr(:, :), work(:)
      Complex (Kind=dp) :: dummy(1)
      Real (Kind=dp) :: rconde(2), rcondv(2)
      Real (Kind=dp), Allocatable :: rwork(:)
      Integer :: idum(1)
      Integer, Allocatable :: iwork(:)
      Logical, Allocatable :: bwork(:)
      Character (1) :: clabs(1), rlabs(1)
!     .. Intrinsic Procedures ..
      Intrinsic :: abs, cmplx, epsilon, max, nint, real
!     .. Executable Statements ..
      Write (nout, *) 'ZGGESX Example Program Results'
      Write (nout, *)
      Flush (nout)
!     Skip heading in data file
      Read (nin, *)
      Read (nin, *) n
      lda = n
      ldb = n
      ldc = n
      ldd = n
      lde = n
      ldvsl = n
      ldvsr = n
      Allocate (a(lda,n), alpha(n), b(ldb,n), beta(n), c(ldc,n), d(ldd,n), &
        e(lde,n), vsl(ldvsl,n), vsr(ldvsr,n), rwork(8*n), bwork(n))

!     Use routine workspace query to get optimal workspace.
      lwork = -1
      liwork = -1
      Call zggesx('Vectors (left)', 'Vectors (right)', 'Sort', selctg, &
        'Both reciprocal condition numbers', n, a, lda, b, ldb, sdim, alpha, &
        beta, vsl, ldvsl, vsr, ldvsr, rconde, rcondv, dummy, lwork, rwork, &
        idum, liwork, bwork, info)

!     Make sure that there is enough workspace for block size nb.
      lwork = max(n*nb+n*n/2, nint(real(dummy(1))))
      liwork = max(n+2, idum(1))
      Allocate (work(lwork), iwork(liwork))

!     Read in the matrices A and B
      Read (nin, *)(a(i,1:n), i=1, n)
      Read (nin, *)(b(i,1:n), i=1, n)

      If (chkfac) Then
!       Copy A and B into D and E respectively
        d(1:n, 1:n) = a(1:n, 1:n)
        e(1:n, 1:n) = b(1:n, 1:n)
      End If

!     Find the Frobenius norms of A and B
      anorm = zlange('Frobenius', n, n, a, lda, rwork)
      bnorm = zlange('Frobenius', n, n, b, ldb, rwork)

      If (prmat) Then
!       Print matrices A and B
!       ifail: behaviour on error exit
!              =0 for hard exit, =1 for quiet-soft, =-1 for noisy-soft
        ifail = 0
        Call nagf_file_print_matrix_complex_gen_comp('General', ' ', n, n, a, &
          lda, 'Bracketed', 'F8.4', 'Matrix A', 'Integer', rlabs, 'Integer', &
          clabs, 80, 0, ifail)
        Write (nout, *)
        Flush (nout)

        ifail = 0
        Call nagf_file_print_matrix_complex_gen_comp('General', ' ', n, n, b, &
          ldb, 'Bracketed', 'F8.4', 'Matrix B', 'Integer', rlabs, 'Integer', &
          clabs, 80, 0, ifail)
        Write (nout, *)
        Flush (nout)
      End If

      factor = .True.
!     Find the generalized Schur form
      Call zggesx('Vectors (left)', 'Vectors (right)', 'Sort', selctg, &
        'Both reciprocal condition numbers', n, a, lda, b, ldb, sdim, alpha, &
        beta, vsl, ldvsl, vsr, ldvsr, rconde, rcondv, work, lwork, rwork, &
        iwork, liwork, bwork, info)

      If (info/=0 .And. info/=(n+2)) Then
        Write (nout, 100) 'Failure in ZGGESX. INFO =', info
        factor = .False.
      Else If (chkfac) Then
!       Compute A - Q*S*Z^H from the factorization of (A,B) and store in
!       matrix D
        alph = cmplx(1, kind=dp)
        bet = cmplx(0, kind=dp)
        Call zgemm('N', 'N', n, n, n, alph, vsl, ldvsl, a, lda, bet, c, ldc)
        alph = cmplx(-1, kind=dp)
        bet = cmplx(1, kind=dp)
        Call zgemm('N', 'C', n, n, n, alph, c, ldc, vsr, ldvsr, bet, d, ldd)

!       Compute B - Q*T*Z^H from the factorization of (A,B) and store in
!       matrix E
        alph = cmplx(1, kind=dp)
        bet = cmplx(0, kind=dp)
        Call zgemm('N', 'N', n, n, n, alph, vsl, ldvsl, b, ldb, bet, c, ldc)
        alph = cmplx(-1, kind=dp)
        bet = cmplx(1, kind=dp)
        Call zgemm('N', 'C', n, n, n, alph, c, ldc, vsr, ldvsr, bet, e, lde)

!       Find norms of matrices D and E and warn if either is too large
        normd = zlange('O', ldd, n, d, ldd, rwork)
        If (normd>epsilon(1.0E0_dp)**0.75_dp) Then
          Write (nout, *) 'Norm of A-(Q*S*Z^T) is much greater than 0.'
          factor = .False.
          Write (nout, *) 'Schur factorization has failed.'
        End If
        norme = zlange('O', lde, n, e, lde, rwork)
        If (norme>epsilon(1.0E0_dp)**0.75_dp) Then
          Write (nout, *) 'Norm of B-(Q*T*Z^T) is much greater than 0.'
          factor = .False.
        End If
      End If

      If (factor) Then
!       Print eigenvalue details
        Write (nout, 100) 'Number of eigenvalues for which SELCTG is true = ', &
          sdim, '(dimension of deflating subspaces)'

        Write (nout, *)
!       Print selected (finite) generalized eigenvalues
        Write (nout, *) 'Selected generalized eigenvalues'

!       Store absolute values of eigenvalues for ranking
        work(1:n) = alpha(1:n)/beta(1:n)
        rwork(1:n) = abs(work(1:n))

!       Rank eigenvalues
        ifail = 0
        Call nagf_sort_realvec_rank(rwork, 1, sdim, 'Descending', iwork, &
          ifail)

!       Sort eigenvalues in work(1:n)
        Call nagf_sort_cmplxvec_rank_rearrange(work, 1, sdim, iwork, ifail)
        Do i = 1, sdim
          Write (nout, 110) i, work(i)
        End Do

        If (info==(n+2)) Then
          Write (nout, 120) '*** Note that rounding errors mean ', &
            'that leading eigenvalues in the', &
            'generalized Schur form no longer satisfy SELCTG = .TRUE.'
          Write (nout, *)
        End If
        Flush (nout)

        If (prcond) Then
!         Compute the machine precision and sqrt(anorm**2+bnorm**2)
          eps = epsilon(1.0E0_dp)
          abnorm = nagf_blas_dpyth(anorm, bnorm)
          tol = eps*abnorm

!         Print out the reciprocal condition numbers and error bound for
!         selected eigenvalues
          Write (nout, *)
          Write (nout, 130) &
            'Reciprocal condition numbers for the average of the', &
            'selected eigenvalues and their asymptotic error bound', &
            'rcond-left = ', rconde(1), ', rcond-right = ', rconde(2), &
            ', error = ', tol/rconde(1)

          Write (nout, *)
          Write (nout, 130) &
            'Reciprocal condition numbers for the deflating subspaces', &
            'and their approximate asymptotic error bound', 'rcond-left = ', &
            rcondv(1), ', rcond-right = ', rcondv(2), ', error = ', &
            tol/rcondv(2)
        End If

      Else
        Write (nout, *) 'Schur factorization has failed.'
      End If

100   Format (1X, A, I4, /, 1X, A)
110   Format (1X, I2, 1X, '(', F6.2, ',', F6.2, ')')
120   Format (1X, 2A, /, 1X, A)
130   Format (1X, A, /, 1X, A, /, 1X, 3(A,1P,E8.1))
    End Program
