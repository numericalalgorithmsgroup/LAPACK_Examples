    Program ztgsna_example

!     ZTGSNA Example Program Text

!     Copyright (c) 2018, Numerical Algorithms Group (NAG Ltd.)
!     For licence see
!       https://github.com/numericalalgorithmsgroup/LAPACK_Examples/blob/master/LICENCE.md

!     .. Use Statements ..
      Use lapack_example_aux, Only: nagf_blas_dpyth
      Use lapack_interfaces, Only: zlange, ztgevc, ztgsna
      Use lapack_precision, Only: dp
!     .. Implicit None Statement ..
      Implicit None
!     .. Parameters ..
      Integer, Parameter :: nin = 5, nout = 6
!     .. Local Scalars ..
      Real (Kind=dp) :: eps, snorm, stnrm, tnorm
      Integer :: i, info, lda, ldb, ldvl, ldvr, lwork, m, n
!     .. Local Arrays ..
      Complex (Kind=dp), Allocatable :: a(:, :), b(:, :), vl(:, :), vr(:, :), &
        work(:)
      Real (Kind=dp), Allocatable :: dif(:), rwork(:), s(:)
      Integer, Allocatable :: iwork(:)
      Logical :: select(1)
!     .. Intrinsic Procedures ..
      Intrinsic :: epsilon
!     .. Executable Statements ..
      Write (nout, *) 'ZTGSNA Example Program Results'
      Write (nout, *)
!     Skip heading in data file
      Read (nin, *)
      Read (nin, *) n
      lda = n
      ldb = n
      ldvl = n
      ldvr = n
      lwork = 2*n*n
      Allocate (a(lda,n), b(ldb,n), vl(ldvl,n), vr(ldvr,n), work(lwork), &
        dif(n), rwork(2*n), s(n), iwork(n+2))

!     Read A and B from data file

      Read (nin, *)(a(i,1:n), i=1, n)
      Read (nin, *)(b(i,1:n), i=1, n)

!     Calculate the left and right generalized eigenvectors of the
!     pair (A,B).

      Call ztgevc('Both', 'All', select, n, a, lda, b, ldb, vl, ldvl, vr, &
        ldvr, n, m, work, rwork, info)

!     Estimate condition numbers for all the generalized eigenvalues
!     and right eigenvectors of the pair (A,B)

      Call ztgsna('Both', 'All', select, n, a, lda, b, ldb, vl, ldvl, vr, &
        ldvr, s, dif, n, m, work, lwork, iwork, info)

!     Print condition numbers of eigenvalues and right eigenvectors

      Write (nout, *) 'S'
      Write (nout, 100) s(1:m)
      Write (nout, *)
      Write (nout, *) 'DIF'
      Write (nout, 100) dif(1:m)

!     Calculate approximate error estimates

!     Compute the 1-norms of A and B and then compute
!     SQRT(snorm**2 + tnorm**2)

      eps = epsilon(1.0E0_dp)
      snorm = zlange('1-norm', n, n, a, lda, rwork)
      tnorm = zlange('1-norm', n, n, b, ldb, rwork)
      stnrm = nagf_blas_dpyth(snorm, tnorm)
      Write (nout, *)
      Write (nout, *) 'Approximate error estimates for eigenvalues of (A,B)'
      Write (nout, 100)(eps*stnrm/s(i), i=1, m)
      Write (nout, *)
      Write (nout, *) &
        'Approximate error estimates for right eigenvectors of (A,B)'
      Write (nout, 100)(eps*stnrm/dif(i), i=1, m)

100   Format ((3X,1P,7E11.1))
    End Program
