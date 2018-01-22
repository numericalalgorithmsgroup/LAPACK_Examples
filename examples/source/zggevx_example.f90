    Program zggevx_example

!     ZGGEVX Example Program Text

!     Copyright 2017, Numerical Algorithms Group Ltd. http://www.nag.com

!     .. Use Statements ..
      Use lapack_example_aux, Only: nagf_sort_realvec_rank_rearrange, &
        nagf_blas_dpyth, nagf_sort_cmplxvec_rank_rearrange, &
        nagf_sort_realvec_rank
      Use lapack_interfaces, Only: zggevx
      Use lapack_precision, Only: dp
!     .. Implicit None Statement ..
      Implicit None
!     .. Parameters ..
      Integer, Parameter :: nb = 64, nin = 5, nout = 6
      Logical, Parameter :: verbose = .False.
!     .. Local Scalars ..
      Complex (Kind=dp) :: eig, scal
      Real (Kind=dp) :: abnorm, abnrm, bbnrm, eps, small, tol
      Integer :: i, ifail, ihi, ilo, info, j, k, lda, ldb, ldvr, lwork, n
!     .. Local Arrays ..
      Complex (Kind=dp), Allocatable :: a(:, :), alpha(:), b(:, :), beta(:), &
        temp(:), vr(:, :), work(:)
      Complex (Kind=dp) :: dummy(1, 1)
      Real (Kind=dp), Allocatable :: lscale(:), rconde(:), rcondv(:), &
        rscale(:), rwork(:)
      Integer, Allocatable :: irank(:), iwork(:)
      Logical, Allocatable :: bwork(:)
!     .. Intrinsic Procedures ..
      Intrinsic :: abs, epsilon, max, maxloc, nint, real, tiny
!     .. Executable Statements ..
      Write (nout, *) 'ZGGEVX Example Program Results'
!     Skip heading in data file
      Read (nin, *)
      Read (nin, *) n
      lda = n
      ldb = n
      ldvr = n
      Allocate (a(lda,n), alpha(n), b(ldb,n), beta(n), vr(ldvr,n), lscale(n), &
        rconde(n), rcondv(n), rscale(n), rwork(6*n), iwork(n+2), bwork(n), &
        temp(n))

!     Use routine workspace query to get optimal workspace.
      lwork = -1
      Call zggevx('Balance', 'No vectors (left)', 'Vectors (right)', &
        'Both reciprocal condition numbers', n, a, lda, b, ldb, alpha, beta, &
        dummy, 1, vr, ldvr, ilo, ihi, lscale, rscale, abnrm, bbnrm, rconde, &
        rcondv, dummy, lwork, rwork, iwork, bwork, info)

!     Make sure that there is enough workspace for block size nb.
      lwork = max((nb+2*n)*n, nint(real(dummy(1,1))))
      Allocate (work(lwork))

!     Read in the matrices A and B

      Read (nin, *)(a(i,1:n), i=1, n)
      Read (nin, *)(b(i,1:n), i=1, n)

!     Solve the generalized eigenvalue problem

      Call zggevx('Balance', 'No vectors (left)', 'Vectors (right)', &
        'Both reciprocal condition numbers', n, a, lda, b, ldb, alpha, beta, &
        dummy, 1, vr, ldvr, ilo, ihi, lscale, rscale, abnrm, bbnrm, rconde, &
        rcondv, work, lwork, rwork, iwork, bwork, info)

      If (info>0) Then
        Write (nout, *)
        Write (nout, 100) 'Failure in ZGGEVX. INFO =', info
      Else

!       Compute the machine precision, the safe range parameter
!       SMALL and sqrt(ABNRM**2+BBNRM**2)

        eps = epsilon(1.0E0_dp)
        small = tiny(1.0E0_dp)
        abnorm = nagf_blas_dpyth(abnrm, bbnrm)
        tol = eps*abnorm

!       Reorder eigenvalues by descending absolute value
        rwork(1:n) = abs(alpha(1:n)/beta(1:n))
        Allocate (irank(n))
        ifail = 0
        Call nagf_sort_realvec_rank(rwork, 1, n, 'Descending', irank, ifail)
        Call nagf_sort_cmplxvec_rank_rearrange(alpha, 1, n, irank, ifail)
        Call nagf_sort_cmplxvec_rank_rearrange(beta, 1, n, irank, ifail)
        Call nagf_sort_realvec_rank_rearrange(rconde, 1, n, irank, ifail)
!       Reorder eigenvectors accordingly
        Do j = 1, n
          temp(1:n) = vr(j, 1:n)
          Call nagf_sort_cmplxvec_rank_rearrange(temp, 1, n, irank, ifail)
          vr(j, 1:n) = temp(1:n)
        End Do
        Call nagf_sort_realvec_rank_rearrange(rcondv, 1, n, irank, ifail)

!       Print out eigenvalues and vectors and associated condition
!       number and bounds

        Write (nout, *)
        Write (nout, *) 'Eigenvalues'
        Write (nout, *)
        If (verbose) Then
          Write (nout, *) '         Eigenvalue           rcond    error'
        Else
          Write (nout, *) '         Eigenvalue'
        End If

        Do j = 1, n

!         Print out information on the j-th eigenvalue

          If ((abs(alpha(j)))*small>=abs(beta(j))) Then
            If (rconde(j)>0.0_dp) Then
              If (tol/rconde(j)<500.0_dp*eps) Then
                Write (nout, 140) j, rconde(j), '-'
              Else
                Write (nout, 150) j, rconde(j), tol/rconde(j)
              End If
            Else
              Write (nout, 140) j, rconde(j), 'Inf'
            End If
          Else
            eig = alpha(j)/beta(j)
            If (verbose) Then
              If (rconde(j)>0.0_dp) Then
                If (tol/rconde(j)<500.0_dp*eps) Then
                  Write (nout, 110) j, eig, rconde(j), '-'
                Else
                  Write (nout, 120) j, eig, rconde(j), tol/rconde(j)
                End If
              Else
                Write (nout, 110) j, eig, rconde(j), 'Inf'
              End If
            Else
              Write (nout, 110) j, eig
            End If
          End If

        End Do

        Write (nout, *)
        Write (nout, *) 'Eigenvectors'
        Write (nout, *)
        If (verbose) Then
          Write (nout, *) '         Eigenvector          rcond    error'
        Else
          Write (nout, *) '         Eigenvector'
        End If

        Do j = 1, n

!         Print information on j-th eigenvector
          Write (nout, *)

!         Re-normalize eigenvector, largest absolute element real (=1)
          rwork(1:n) = abs(vr(1:n,j))
          k = maxloc(rwork(1:n), 1)
          scal = (1.0_dp, 0.0_dp)/vr(k, j)
          vr(1:n, j) = vr(1:n, j)*scal

          If (verbose) Then
            If (rcondv(j)>0.0_dp) Then
              If (tol/rcondv(j)<500.0_dp*eps) Then
                Write (nout, 110) j, vr(1, j), rcondv(j), '-'
              Else
                Write (nout, 120) j, vr(1, j), rcondv(j), tol/rcondv(j)
              End If
            Else
              Write (nout, 110) j, vr(1, j), rcondv(j), 'Inf'
            End If
          Else
            Write (nout, 110) j, vr(1, j)
          End If
          Write (nout, 130) vr(2:n, j)

        End Do

        If (verbose) Then
          Write (nout, *)
          Write (nout, *) &
            'Errors below 500*machine precision are not displayed'
        End If
      End If

100   Format (1X, A, I4)
110   Format (1X, I2, 1X, '(', 1P, E11.4, ',', E11.4, ')', 1X, 0P, F7.4, 4X, &
        A)
120   Format (1X, I2, 1X, '(', 1P, E11.4, ',', E11.4, ')', 1X, 0P, F7.4, 1X, &
        1P, E8.1)
130   Format (1X, 3X, '(', 1P, E11.4, ',', E11.4, ')')
140   Format (1X, I2, 1X, '  Infinite or undetermined', 1X, 0P, F7.4, 4X, A)
150   Format (1X, I2, 1X, '  Infinite or undetermined', 1X, 0P, F7.4, 1X, 1P, &
        E8.1)

    End Program
