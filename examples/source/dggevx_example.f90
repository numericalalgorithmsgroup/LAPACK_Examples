    Program dggevx_example

!     DGGEVX Example Program Text

!     Copyright (c) 2018, Numerical Algorithms Group (NAG Ltd.)
!     For licence see
!       https://github.com/numericalalgorithmsgroup/LAPACK_Examples/blob/master/LICENCE.md

!     .. Use Statements ..
      Use blas_interfaces, Only: dnrm2
      Use lapack_example_aux, Only: nagf_sort_realvec_rank, &
        nagf_sort_realvec_rank_rearrange
      Use lapack_interfaces, Only: dggevx
      Use lapack_precision, Only: dp
!     .. Implicit None Statement ..
      Implicit None
!     .. Parameters ..
      Integer, Parameter :: nb = 64, nin = 5, nout = 6
      Logical, Parameter :: verbose = .False.
!     .. Local Scalars ..
      Complex (Kind=dp) :: eig
      Real (Kind=dp) :: abnrm, bbnrm, eps, jswap, rcnd, scal_i, scal_r, small
      Integer :: i, ifail, ihi, ilo, info, j, k, lda, ldb, ldvr, lwork, n
      Logical :: pair
!     .. Local Arrays ..
      Real (Kind=dp), Allocatable :: a(:, :), alphai(:), alphar(:), b(:, :), &
        beta(:), lscale(:), rconde(:), rcondv(:), rscale(:), vr(:, :), &
        vr_row(:), work(:)
      Real (Kind=dp) :: dummy(1, 1)
      Integer, Allocatable :: iwork(:)
      Logical, Allocatable :: bwork(:)
!     .. Intrinsic Procedures ..
      Intrinsic :: abs, all, cmplx, epsilon, max, maxloc, nint, sqrt, sum, &
        tiny
!     .. Executable Statements ..
      Write (nout, *) 'DGGEVX Example Program Results'
!     Skip heading in data file
      Read (nin, *)
      Read (nin, *) n
      lda = n
      ldb = n
      ldvr = n
      Allocate (a(lda,n), alphai(n), alphar(n), b(ldb,n), beta(n), lscale(n), &
        rconde(n), rcondv(n), rscale(n), vr(ldvr,n), iwork(n+6), bwork(n))

!     Use routine workspace query to get optimal workspace.
      lwork = -1
      Call dggevx('Balance', 'No vectors (left)', 'Vectors (right)', &
        'Both reciprocal condition numbers', n, a, lda, b, ldb, alphar, &
        alphai, beta, dummy, 1, vr, ldvr, ilo, ihi, lscale, rscale, abnrm, &
        bbnrm, rconde, rcondv, dummy, lwork, iwork, bwork, info)

!     Make sure that there is enough workspace for block size nb.
      lwork = max((nb+2*n)*n, nint(dummy(1,1)))
      Allocate (work(lwork))

!     Read in the matrices A and B

      Read (nin, *)(a(i,1:n), i=1, n)
      Read (nin, *)(b(i,1:n), i=1, n)

!     Solve the generalized eigenvalue problem

      Call dggevx('Balance', 'No vectors (left)', 'Vectors (right)', &
        'Both reciprocal condition numbers', n, a, lda, b, ldb, alphar, &
        alphai, beta, dummy, 1, vr, ldvr, ilo, ihi, lscale, rscale, abnrm, &
        bbnrm, rconde, rcondv, work, lwork, iwork, bwork, info)

      If (info>0) Then
        Write (nout, *)
        Write (nout, 100) 'Failure in DGGEVX. INFO =', info
      Else

!       Compute the machine precision and the safe range parameter
!       small

        eps = epsilon(1.0E0_dp)
        small = tiny(1.0E0_dp)

!       If beta(:) > eps, Order eigenvalues by ascending real parts
        If (all(abs(beta(1:n))>eps)) Then
          work(1:n) = alphar(1:n)/beta(1:n)
          ifail = 0
          Call nagf_sort_realvec_rank(work, 1, n, 'Ascending', iwork, ifail)
          Call nagf_sort_realvec_rank_rearrange(alphar, 1, n, iwork, ifail)
          Call nagf_sort_realvec_rank_rearrange(alphai, 1, n, iwork, ifail)
          Call nagf_sort_realvec_rank_rearrange(beta, 1, n, iwork, ifail)
!         Order the eigenvectors in the same way
          Allocate (vr_row(n))
          Do j = 1, n
            vr_row(1:n) = vr(j, 1:n)
            Call nagf_sort_realvec_rank_rearrange(vr_row, 1, n, iwork, ifail)
            vr(j, 1:n) = vr_row(1:n)
          End Do
          Deallocate (vr_row)
        End If

!       Print out eigenvalues and vectors and associated condition
!       number and bounds

        pair = .False.
        Do j = 1, n

!         Print out information on the j-th eigenvalue

          Write (nout, *)
          If ((abs(alphar(j))+abs(alphai(j)))*small>=abs(beta(j))) Then
            Write (nout, 110) 'Eigenvalue(', j, ')', &
              ' is numerically infinite or undetermined', 'ALPHAR(', j, &
              ') = ', alphar(j), ', ALPHAI(', j, ') = ', alphai(j), ', BETA(', &
              j, ') = ', beta(j)
          Else
            If (.Not. pair) Then
              jswap = 1.0_dp
              If (alphai(j)>0.0_dp) Then
                jswap = -jswap
              End If
            End If
            If (alphai(j)==0.0E0_dp) Then
              Write (nout, 120) 'Eigenvalue(', j, ') = ', alphar(j)/beta(j)
            Else
              eig = cmplx(alphar(j), jswap*alphai(j), kind=dp)/ &
                cmplx(beta(j), kind=dp)
              Write (nout, 130) 'Eigenvalue(', j, ') = ', eig
            End If
          End If

          If (verbose) Then
            rcnd = rconde(j)
            Write (nout, *)
            Write (nout, 140) '  Reciprocal condition number = ', rcnd
          End If

!         Print out information on the j-th eigenvector

          Write (nout, *)
          Write (nout, 150) 'Eigenvector(', j, ')'
          If (alphai(j)==0.0E0_dp) Then
!           Let largest element be positive
            work(1:n) = abs(vr(1:n,j))
            k = maxloc(work(1:n), 1)
            If (vr(k,j)<0.0_dp) Then
              vr(1:n, j) = -vr(1:n, j)/dnrm2(n, vr(1,j), 1)
            End If
            Write (nout, 160)(vr(i,j), i=1, n)
          Else
            If (pair) Then
              Write (nout, 170)(vr(i,j-1), -jswap*vr(i,j), i=1, n)
            Else
!             Let largest element be real (and positive).
              work(1:n) = vr(1:n, j)**2 + vr(1:n, j+1)**2
              k = maxloc(work(1:n), 1)
              scal_r = vr(k, j)/sqrt(work(k))/sqrt(sum(work(1:n)))
              scal_i = -vr(k, j+1)/sqrt(work(k))/sqrt(sum(work(1:n)))
              work(1:n) = vr(1:n, j)
              vr(1:n, j) = scal_r*work(1:n) - scal_i*vr(1:n, j+1)
              vr(1:n, j+1) = scal_r*vr(1:n, j+1) + scal_i*work(1:n)
              vr(k, j+1) = 0.0_dp
              Write (nout, 170)(vr(i,j), jswap*vr(i,j+1), i=1, n)
            End If
            pair = .Not. pair
          End If
          If (verbose) Then
            rcnd = rcondv(j)
            Write (nout, *)
            Write (nout, 140) '  Reciprocal condition number = ', rcnd
          End If
        End Do

      End If

100   Format (1X, A, I4)
110   Format (/, 1X, A, I2, 2A, /, 1X, 2(A,I2,A,F11.5), A, I2, A, F11.5)
120   Format (/, 1X, A, I2, A, F11.5)
130   Format (/, 1X, A, I2, A, '(', F11.5, ',', F11.5, ')')
140   Format (1X, A, 1P, E8.1)
150   Format (1X, A, I2, A)
160   Format (1X, F11.5)
170   Format (1X, '(', F11.5, ',', F11.5, ')')
    End Program
