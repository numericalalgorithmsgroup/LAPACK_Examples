    Program dtrsna_example

!     DTRSNA Example Program Text

!     Copyright (c) 2018, Numerical Algorithms Group (NAG Ltd.)
!     For licence see
!       https://github.com/numericalalgorithmsgroup/LAPACK_Examples/blob/master/LICENCE.md

!     .. Use Statements ..
      Use lapack_interfaces, Only: dlange, dtrevc, dtrsna
      Use lapack_precision, Only: dp
!     .. Implicit None Statement ..
      Implicit None
!     .. Parameters ..
      Integer, Parameter :: nin = 5, nout = 6
!     .. Local Scalars ..
      Real (Kind=dp) :: eps, tnorm
      Integer :: i, info, ldt, ldvl, ldvr, ldwork, m, n
!     .. Local Arrays ..
      Real (Kind=dp), Allocatable :: s(:), sep(:), t(:, :), vl(:, :), &
        vr(:, :), work(:, :)
      Integer, Allocatable :: iwork(:)
      Logical :: select(1)
!     .. Intrinsic Procedures ..
      Intrinsic :: epsilon
!     .. Executable Statements ..
      Write (nout, *) 'DTRSNA Example Program Results'
      Write (nout, *)
!     Skip heading in data file
      Read (nin, *)
      Read (nin, *) n
      ldt = n
      ldvl = n
      ldvr = n
      ldwork = n
      Allocate (s(n), sep(n), t(ldt,n), vl(ldvl,n), vr(ldvr,n), &
        work(ldwork,n+6), iwork(2*n-1))

!     Read T from data file

      Read (nin, *)(t(i,1:n), i=1, n)

!     Calculate the left and right eigenvectors of T

      Call dtrevc('Both', 'All', select, n, t, ldt, vl, ldvl, vr, ldvr, n, m, &
        work, info)

!     Estimate condition numbers for all the eigenvalues and right
!     eigenvectors of T

      Call dtrsna('Both', 'All', select, n, t, ldt, vl, ldvl, vr, ldvr, s, &
        sep, n, m, work, ldwork, iwork, info)

!     Print condition numbers of eigenvalues and right eigenvectors

      Write (nout, *) 'S'
      Write (nout, 100) s(1:m)
      Write (nout, *)
      Write (nout, *) 'SEP'
      Write (nout, 100) sep(1:m)

!     Calculate approximate error estimates (using the 1-norm)

      eps = epsilon(1.0E0_dp)
      tnorm = dlange('1-norm', n, n, t, ldt, work)
      Write (nout, *)
      Write (nout, *) 'Approximate error estimates for eigenvalues ', &
        'of T (machine-dependent)'
      Write (nout, 100)(eps*tnorm/s(i), i=1, m)
      Write (nout, *)
      Write (nout, *) 'Approximate error estimates for right ', &
        'eigenvectors of T (machine-dependent)'
      Write (nout, 100)(eps*tnorm/sep(i), i=1, m)

100   Format ((3X,1P,7E11.1))
    End Program
