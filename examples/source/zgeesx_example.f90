!   ZGEESX Example Program Text
!   Copyright (c) 2018, Numerical Algorithms Group (NAG Ltd.)
!   For licence see
!     https://github.com/numericalalgorithmsgroup/LAPACK_Examples/blob/master/LICENCE.md

    Module zgeesx_example_mod

!     ZGEESX Example Program Module:
!            Parameters and User-defined Routines

!     .. Use Statements ..
      Use lapack_precision, Only: dp
!     .. Implicit None Statement ..
      Implicit None
!     .. Accessibility Statements ..
      Private
      Public :: select
!     .. Parameters ..
      Integer, Parameter, Public :: nb = 64, nin = 5, nout = 6
      Logical, Parameter, Public :: check_fac = .True., print_cond = .False.
    Contains
      Function select(w)

!       Logical function select for use with ZGEESX (ZGEESX)
!       Returns the value .TRUE. if the real part of the eigenvalue
!       w is positive.

!       .. Function Return Value ..
        Logical :: select
!       .. Scalar Arguments ..
        Complex (Kind=dp), Intent (In) :: w
!       .. Intrinsic Procedures ..
        Intrinsic :: real
!       .. Executable Statements ..
        select = (real(w)>0._dp)
        Return
      End Function
    End Module
    Program zgeesx_example

!     ZGEESX Example Main Program

!     .. Use Statements ..
      Use blas_interfaces, Only: zgemm
      Use zgeesx_example_mod, Only: check_fac, nb, nin, nout, print_cond, select
      Use lapack_example_aux, Only: nagf_file_print_matrix_complex_gen_comp
      Use lapack_interfaces, Only: zgeesx, zlange
      Use lapack_precision, Only: dp
!     .. Implicit None Statement ..
      Implicit None
!     .. Local Scalars ..
      Complex (Kind=dp) :: alpha, beta
      Real (Kind=dp) :: anorm, eps, norm, rconde, rcondv, tol
      Integer :: i, ifail, info, lda, ldc, ldd, ldvs, lwork, n, sdim
!     .. Local Arrays ..
      Complex (Kind=dp), Allocatable :: a(:, :), c(:, :), d(:, :), vs(:, :), &
        w(:), work(:)
      Complex (Kind=dp) :: dummy(1)
      Real (Kind=dp), Allocatable :: rwork(:)
      Logical, Allocatable :: bwork(:)
      Character (1) :: clabs(1), rlabs(1)
!     .. Intrinsic Procedures ..
      Intrinsic :: cmplx, epsilon, max, nint, real
!     .. Executable Statements ..
      Write (nout, *) 'ZGEESX Example Program Results'
      Write (nout, *)
      Flush (nout)
!     Skip heading in data file
      Read (nin, *)
      Read (nin, *) n
      lda = n
      ldc = n
      ldd = n
      ldvs = n
      Allocate (a(lda,n), c(ldc,n), d(ldd,n), vs(ldvs,n), w(n), rwork(n), &
        bwork(n))

!     Use routine workspace query to get optimal workspace.
      lwork = -1
      Call zgeesx('Vectors (Schur)', 'Sort', select, &
        'Both reciprocal condition numbers', n, a, lda, sdim, w, vs, ldvs, &
        rconde, rcondv, dummy, lwork, rwork, bwork, info)

!     Make sure that there is enough workspace for block size nb.
      lwork = max(n*(nb+1+n/2), nint(real(dummy(1))))
      Allocate (work(lwork))

!     Read in the matrix A
      Read (nin, *)(a(i,1:n), i=1, n)

!     Copy A into D
      d(1:n, 1:n) = a(1:n, 1:n)

!     Print matrix A
!     ifail: behaviour on error exit
!            =0 for hard exit, =1 for quiet-soft, =-1 for noisy-soft
      ifail = 0
      Call nagf_file_print_matrix_complex_gen_comp('General', ' ', n, n, a, &
        lda, 'Bracketed', 'F7.4', 'Matrix A', 'Integer', rlabs, 'Integer', &
        clabs, 80, 0, ifail)

      Write (nout, *)
      Flush (nout)

!     Find the Frobenius norm of A
      anorm = zlange('Frobenius', n, n, a, lda, rwork)

!     Find the Schur factorization of A
      Call zgeesx('Vectors (Schur)', 'Sort', select, &
        'Both reciprocal condition numbers', n, a, lda, sdim, w, vs, ldvs, &
        rconde, rcondv, work, lwork, rwork, bwork, info)

      If (info/=0 .And. info/=(n+2)) Then
        Write (nout, 170) 'Failure in ZGEESX. INFO =', info
        Go To 100
      End If

      If (check_fac) Then
!       Compute A - Z*T*Z^H from the factorization of A and store in matrix D
        alpha = cmplx(1, kind=dp)
        beta = cmplx(0, kind=dp)
        Call zgemm('N', 'N', n, n, n, alpha, vs, ldvs, a, lda, beta, c, ldc)
        alpha = cmplx(-1, kind=dp)
        beta = cmplx(1, kind=dp)
        Call zgemm('N', 'C', n, n, n, alpha, c, ldc, vs, ldvs, beta, d, ldd)

!       Find norm of matrix D and print warning if it is too large
        norm = zlange('O', ldd, n, d, ldd, rwork)
        If (norm>epsilon(1.0E0_dp)**0.5_dp) Then
          Write (nout, *) 'Norm of A-(Z*T*Z^H) is much greater than 0.'
          Write (nout, *) 'Schur factorization has failed.'
          Go To 100
        End If
      End If

!     Print solution
      Write (nout, 110) 'Number of eigenvalues for which SELECT is true = ', &
        sdim, '(dimension of invariant subspace)'

      Write (nout, *)
!     Print eigenvalues.
      Write (nout, *) 'Selected eigenvalues'
      Write (nout, 120)(i, w(i), i=1, sdim)
      Write (nout, *)

      If (info==(n+2)) Then
        Write (nout, 130) '***Note that rounding errors mean ', &
          'that leading eigenvalues in the Schur form', &
          'no longer satisfy SELECT = .TRUE.'
        Write (nout, *)
      End If
      Flush (nout)

      If (print_cond) Then
!       Print out the reciprocal condition numbers
        Write (nout, 140) 'Reciprocal of projection norm onto the invariant', &
          'subspace for the selected eigenvalues', 'RCONDE = ', rconde
        Write (nout, *)
        Write (nout, 150) &
          'Reciprocal condition number for the invariant subspace', &
          'RCONDV = ', rcondv

!       Compute the machine precision
        eps = epsilon(1.0E0_dp)
        tol = eps*anorm

!       Print out the approximate asymptotic error bound on the
!       average absolute error of the selected eigenvalues given by
!       eps*norm(A)/RCONDE
        Write (nout, *)
        Write (nout, 160) 'Approximate asymptotic error bound for selected ', &
          'eigenvalues   = ', tol/rconde

!       Print out an approximate asymptotic bound on the maximum
!       angular error in the computed invariant subspace given by
!       eps*norm(A)/RCONDV
        Write (nout, 160) &
          'Approximate asymptotic error bound for the invariant ', &
          'subspace = ', tol/rcondv
      End If
100   Continue

110   Format (1X, A, I4, /, 1X, A)
120   Format (1X, I4, 2X, ' (', F7.4, ',', F7.4, ')', :)
130   Format (1X, 2A, /, 1X, A)
140   Format (1X, A, /, 1X, A, /, 1X, A, 1P, E8.1)
150   Format (1X, A, /, 1X, A, 1P, E8.1)
160   Format (1X, 2A, 1P, E8.1)
170   Format (1X, A, I4)
    End Program
