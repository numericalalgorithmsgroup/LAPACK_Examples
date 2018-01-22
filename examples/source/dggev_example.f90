    Program dggev_example

!     DGGEV Example Program Text

!     Copyright 2017, Numerical Algorithms Group Ltd. http://www.nag.com

!     .. Use Statements ..
      Use lapack_example_aux, Only: nagf_sort_realmat_rank_rows, &
        nagf_sort_realvec_rank_rearrange
      Use lapack_interfaces, Only: dggev
      Use lapack_precision, Only: dp
!     .. Implicit None Statement ..
      Implicit None
!     .. Parameters ..
      Real (Kind=dp), Parameter :: zero = 0.0_dp
      Integer, Parameter :: nb = 64, nin = 5, nout = 6
!     .. Local Scalars ..
      Complex (Kind=dp) :: eig
      Real (Kind=dp) :: scal_i, scal_r, small
      Integer :: i, ifail, info, j, k, lda, ldb, ldvr, lwork, n
      Logical :: pair
!     .. Local Arrays ..
      Real (Kind=dp), Allocatable :: a(:, :), alphai(:), alphar(:), b(:, :), &
        beta(:), vr(:, :), vr_row(:), work(:)
      Real (Kind=dp) :: dummy(1, 1)
      Integer, Allocatable :: irank(:)
!     .. Intrinsic Procedures ..
      Intrinsic :: abs, all, cmplx, epsilon, max, maxloc, nint, sqrt, tiny
!     .. Executable Statements ..
      Write (nout, *) 'DGGEV Example Program Results'
!     Skip heading in data file
      Read (nin, *)
      Read (nin, *) n
      lda = n
      ldb = n
      ldvr = n
      Allocate (a(lda,n), alphai(n), alphar(n), b(ldb,n), beta(n), vr(ldvr,n), &
        irank(n))

!     Use routine workspace query to get optimal workspace.
      lwork = -1
      Call dggev('No left vectors', 'Vectors (right)', n, a, lda, b, ldb, &
        alphar, alphai, beta, dummy, 1, vr, ldvr, dummy, lwork, info)

!     Make sure that there is enough workspace for block size nb.
      lwork = max((nb+7)*n, nint(dummy(1,1)))
      Allocate (work(lwork))

!     Read in the matrices A and B

      Read (nin, *)(a(i,1:n), i=1, n)
      Read (nin, *)(b(i,1:n), i=1, n)

!     Solve the generalized eigenvalue problem

      Call dggev('No left vectors', 'Vectors (right)', n, a, lda, b, ldb, &
        alphar, alphai, beta, dummy, 1, vr, ldvr, work, lwork, info)

      If (info>0) Then
        Write (nout, *)
        Write (nout, 100) 'Failure in DGGEV. INFO =', info
      Else
!       If beta(:) > eps, Order eigenvalues by ascending real parts
!       and then by ascending imaginary parts
        If (all(abs(beta(1:n))>epsilon(1.0E0_dp))) Then
          work(1:n) = alphar(1:n)/beta(1:n)
          work(n+1:2*n) = alphai(1:n)/beta(1:n)
          ifail = 0
          Call nagf_sort_realmat_rank_rows(work, n, 1, n, 1, 2, 'Ascending', &
            irank, ifail)
          Call nagf_sort_realvec_rank_rearrange(alphar, 1, n, irank, ifail)
          Call nagf_sort_realvec_rank_rearrange(alphai, 1, n, irank, ifail)
          Call nagf_sort_realvec_rank_rearrange(beta, 1, n, irank, ifail)
!         Order the eigenvectors in the same way
          Allocate (vr_row(n))
          Do j = 1, n
            vr_row(1:n) = vr(j, 1:n)
            Call nagf_sort_realvec_rank_rearrange(vr_row, 1, n, irank, ifail)
            vr(j, 1:n) = vr_row(1:n)
          End Do
          Deallocate (vr_row)
        End If
        small = tiny(1.0E0_dp)
        pair = .False.
        Do j = 1, n
          Write (nout, *)
          If ((abs(alphar(j))+abs(alphai(j)))*small>=abs(beta(j))) Then
            Write (nout, 110) 'Eigenvalue(', j, ')', &
              ' is numerically infinite or undetermined', 'ALPHAR(', j, &
              ') = ', alphar(j), ', ALPHAI(', j, ') = ', alphai(j), ', BETA(', &
              j, ') = ', beta(j)
          Else
            If (alphai(j)==zero) Then
              Write (nout, 120) 'Eigenvalue(', j, ') = ', alphar(j)/beta(j)
            Else
              eig = cmplx(alphar(j), alphai(j), kind=dp)/ &
                cmplx(beta(j), kind=dp)
              Write (nout, 130) 'Eigenvalue(', j, ') = ', eig
            End If
          End If
          Write (nout, *)
          Write (nout, 140) 'Eigenvector(', j, ')'
          If (alphai(j)==zero) Then
!           Let largest element be positive
            work(1:n) = abs(vr(1:n,j))
            k = maxloc(work(1:n), 1)
            If (vr(k,j)<zero) Then
              vr(1:n, j) = -vr(1:n, j)
            End If
            Write (nout, 150)(vr(i,j), i=1, n)
          Else
            If (pair) Then
              Write (nout, 160)(vr(i,j-1), -vr(i,j), i=1, n)
            Else
!             Let largest element be real (and positive).
              work(1:n) = vr(1:n, j)**2 + vr(1:n, j+1)**2
              k = maxloc(work(1:n), 1)
              scal_r = vr(k, j)/sqrt(work(k))
              scal_i = -vr(k, j+1)/sqrt(work(k))
              work(1:n) = vr(1:n, j)
              vr(1:n, j) = scal_r*work(1:n) - scal_i*vr(1:n, j+1)
              vr(1:n, j+1) = scal_r*vr(1:n, j+1) + scal_i*work(1:n)
              Write (nout, 160)(vr(i,j), vr(i,j+1), i=1, n)
            End If
            pair = .Not. pair
          End If
        End Do

      End If

100   Format (1X, A, I4)
110   Format (1X, A, I2, 2A, /, 1X, 2(A,I2,A,1P,F11.3,3X), A, I2, A, 1P, &
        F11.3)
120   Format (1X, A, I2, A, 1P, F11.3)
130   Format (1X, A, I2, A, '(', 1P, F11.3, ',', 1P, F11.3, ')')
140   Format (1X, A, I2, A)
150   Format (1X, 1P, F11.5)
160   Format (1X, '(', 1P, F11.5, ',', 1P, F11.5, ')')
    End Program
