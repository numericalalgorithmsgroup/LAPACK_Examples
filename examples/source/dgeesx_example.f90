!   DGEESX Example Program Text
!   Copyright (c) 2018, Numerical Algorithms Group (NAG Ltd.)
!   For licence see
!     https://github.com/numericalalgorithmsgroup/LAPACK_Examples/blob/master/LICENCE.md

    Module dgeesx_example_mod

!     DGEESX Example Program Module:
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
      Function select(wr, wi)

!       Logical function select for use with DGEESX (DGEESX)
!       Returns the value .TRUE. if the eigenvalue is real and positive

!       .. Function Return Value ..
        Logical :: select
!       .. Scalar Arguments ..
        Real (Kind=dp), Intent (In) :: wi, wr
!       .. Executable Statements ..
        select = (wr>0._dp .And. wi==0._dp)
        Return
      End Function
    End Module
    Program dgeesx_example

!     DGEESX Example Main Program

!     .. Use Statements ..
      Use blas_interfaces, Only: dgemm
      Use dgeesx_example_mod, Only: check_fac, nb, nin, nout, print_cond, select
      Use lapack_example_aux, Only: nagf_file_print_matrix_real_gen
      Use lapack_interfaces, Only: dgeesx, dlange
      Use lapack_precision, Only: dp
!     .. Implicit None Statement ..
      Implicit None
!     .. Local Scalars ..
      Real (Kind=dp) :: alpha, anorm, beta, eps, norm, rconde, rcondv, tol
      Integer :: i, ifail, info, lda, ldc, ldd, ldvs, liwork, lwork, n, sdim
!     .. Local Arrays ..
      Real (Kind=dp), Allocatable :: a(:, :), c(:, :), d(:, :), vs(:, :), &
        wi(:), work(:), wr(:)
      Real (Kind=dp) :: dummy(1)
      Integer :: idum(1)
      Integer, Allocatable :: iwork(:)
      Logical, Allocatable :: bwork(:)
!     .. Intrinsic Procedures ..
      Intrinsic :: epsilon, max, nint
!     .. Executable Statements ..
      Write (nout, *) 'DGEESX Example Program Results'
      Write (nout, *)
      Flush (nout)
!     Skip heading in data file
      Read (nin, *)
      Read (nin, *) n
      lda = n
      ldc = n
      ldd = n
      ldvs = n
      Allocate (a(lda,n), c(ldc,n), d(ldd,n), vs(ldvs,n), wi(n), wr(n), &
        bwork(n))

!     Use routine workspace query to get optimal workspace.
      lwork = -1
      liwork = -1
      Call dgeesx('Vectors (Schur)', 'Sort', select, &
        'Both reciprocal condition numbers', n, a, lda, sdim, wr, wi, vs, &
        ldvs, rconde, rcondv, dummy, lwork, idum, liwork, bwork, info)

!     Make sure that there is enough workspace for block size nb.
      liwork = max((n*n)/4, idum(1))
      lwork = max(n*(nb+2+n/2), nint(dummy(1)))
      Allocate (work(lwork), iwork(liwork))

!     Read in the matrix A
      Read (nin, *)(a(i,1:n), i=1, n)

!     Copy A into D
      d(1:n, 1:n) = a(1:n, 1:n)

!     Print matrix A
!     ifail: behaviour on error exit
!            =0 for hard exit, =1 for quiet-soft, =-1 for noisy-soft
      ifail = 0
      Call nagf_file_print_matrix_real_gen('General', ' ', n, n, a, lda, &
        'Matrix A', ifail)

      Write (nout, *)
      Flush (nout)

!     Find the Frobenius norm of A
      anorm = dlange('Frobenius', n, n, a, lda, work)

!     Find the Schur factorization of A
      Call dgeesx('Vectors (Schur)', 'Sort', select, &
        'Both reciprocal condition numbers', n, a, lda, sdim, wr, wi, vs, &
        ldvs, rconde, rcondv, work, lwork, iwork, liwork, bwork, info)

      If (info/=0 .And. info/=(n+2)) Then
        Write (nout, 170) 'Failure in DGEESX. INFO =', info
        Go To 100
      End If

      If (check_fac) Then
!       Compute A - Z*T*Z^T from the factorization of A and store in matrix D
        alpha = 1.0_dp
        beta = 0.0_dp
        Call dgemm('N', 'N', n, n, n, alpha, vs, ldvs, a, lda, beta, c, ldc)
        alpha = -1.0_dp
        beta = 1.0_dp
        Call dgemm('N', 'T', n, n, n, alpha, c, ldc, vs, ldvs, beta, d, ldd)

!       Find norm of matrix D and print warning if it is too large
        norm = dlange('O', ldd, n, d, ldd, work)
        If (norm>epsilon(1.0E0_dp)**0.8_dp) Then
          Write (nout, *) 'Norm of A-(Z*T*Z^T) is much greater than 0.'
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
      Write (nout, 120)(' (', wr(i), ',', wi(i), ')', i=1, sdim)
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
120   Format (1X, A, F8.4, A, F8.4, A)
130   Format (1X, 2A, /, 1X, A)
140   Format (1X, A, /, 1X, A, /, 1X, A, 1P, E8.1)
150   Format (1X, A, /, 1X, A, 1P, E8.1)
160   Format (1X, 2A, 1P, E8.1)
170   Format (1X, A, I4)
    End Program
