    Program dtgsen_example

!     DTGSEN Example Program Text

!     Copyright (c) 2018, Numerical Algorithms Group (NAG Ltd.)
!     For licence see
!       https://github.com/numericalalgorithmsgroup/LAPACK_Examples/blob/master/LICENCE.md

!     .. Use Statements ..
      Use lapack_interfaces, Only: dtgsen
      Use lapack_precision, Only: dp
!     .. Implicit None Statement ..
      Implicit None
!     .. Parameters ..
      Integer, Parameter :: nin = 5, nout = 6
!     .. Local Scalars ..
      Real (Kind=dp) :: pl, pr
      Integer :: i, ijob, info, lda, ldb, ldc, ldq, ldz, liwork, lwork, m, n
      Logical :: wantq, wantz
!     .. Local Arrays ..
      Real (Kind=dp), Allocatable :: a(:, :), alphai(:), alphar(:), b(:, :), &
        beta(:), c(:, :), q(:, :), work(:), z(:, :)
      Real (Kind=dp) :: dif(2)
      Integer, Allocatable :: iwork(:)
      Logical, Allocatable :: select(:)
!     .. Executable Statements ..
      Write (nout, *) 'DTGSEN Example Program Results'
      Write (nout, *)
      Flush (nout)
!     Skip heading in data file
      Read (nin, *)
      Read (nin, *) n
      lda = n
      ldb = n
      ldc = n
      ldq = n
      ldz = n
      liwork = (n*n)/2 + 6
      lwork = n*(n+4) + 16
      Allocate (a(lda,n), alphai(n), alphar(n), b(ldb,n), beta(n), c(ldc,n), &
        q(ldq,n), work(lwork), z(ldz,n), iwork(liwork), select(n))

!     Read A, B, Q, Z and the logical array SELECT from data file

      Read (nin, *)(a(i,1:n), i=1, n)
      Read (nin, *)(b(i,1:n), i=1, n)
      Read (nin, *)(q(i,1:n), i=1, n)
      Read (nin, *)(z(i,1:n), i=1, n)

      Read (nin, *) select(1:n)

!     Set ijob, wantq and wantz
      ijob = 4
      wantq = .True.
      wantz = .True.

!     Reorder the Schur factors A and B and update the matrices
!     Q and Z

      Call dtgsen(ijob, wantq, wantz, select, n, a, lda, b, ldb, alphar, &
        alphai, beta, q, ldq, z, ldz, m, pl, pr, dif, work, lwork, iwork, &
        liwork, info)

      If (info>0) Then
        Write (nout, 100) info
        Write (nout, *)
        Flush (nout)
      End If

!     Print Results
      Write (nout, 130) 'Number of selected eigenvalues  = ', m
      Write (nout, *)
      Write (nout, *) 'Selected Generalized Eigenvalues'
      Write (nout, *)
      Write (nout, 120)(i, alphar(i)/beta(i), alphai(i)/beta(i), i=1, m)

      Write (nout, *)
      Write (nout, 110) 'Norm estimate of projection onto', &
        ' left  eigenspace for selected cluster', 1.0_dp/pl
      Write (nout, *)
      Write (nout, 110) 'Norm estimate of projection onto', &
        ' right eigenspace for selected cluster', 1.0_dp/pr
      Write (nout, *)
      Write (nout, 110) 'F-norm based upper bound on', ' Difu', dif(1)
      Write (nout, *)
      Write (nout, 110) 'F-norm based upper bound on', ' Difl', dif(2)

100   Format (' Reordering could not be completed. INFO = ', I3)
110   Format (1X, 2A, /, 1X, 1P, E10.2)
120   Format (1X, I2, 1X, '(', 1P, E11.4, ',', E11.4, ')')
130   Format (1X, A, I4)
    End Program
