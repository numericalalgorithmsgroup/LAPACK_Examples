    Program dtgsna_example

!     DTGSNA Example Program Text

!     Copyright (c) 2018, Numerical Algorithms Group (NAG Ltd.)
!     For licence see
!       https://github.com/numericalalgorithmsgroup/LAPACK_Examples/blob/master/LICENCE.md

!     .. Use Statements ..
      Use lapack_example_aux, Only: nagf_blas_dpyth
      Use lapack_interfaces, Only: dlange, dtgevc, dtgsna
      Use lapack_precision, Only: dp
!     .. Implicit None Statement ..
      Implicit None
!     .. Parameters ..
      Integer, Parameter :: nin = 5, nout = 6
!     .. Local Scalars ..
      Real (Kind=dp) :: eps, snorm, stnrm, tnorm
      Integer :: i, info, lda, ldb, ldvl, ldvr, lwork, m, n
!     .. Local Arrays ..
      Real (Kind=dp), Allocatable :: a(:, :), b(:, :), dif(:), s(:), vl(:, :), &
        vr(:, :), work(:)
      Integer, Allocatable :: iwork(:)
      Logical :: select(1)
!     .. Intrinsic Procedures ..
      Intrinsic :: epsilon
!     .. Executable Statements ..
      Write (nout, *) 'DTGSNA Example Program Results'
      Write (nout, *)
!     Skip heading in data file
      Read (nin, *)
      Read (nin, *) n
      lda = n
      ldb = n
      ldvl = n
      ldvr = n
      lwork = 2*n*(n+2) + 16
      Allocate (a(lda,n), b(ldb,n), dif(n), s(n), vl(ldvl,n), vr(ldvr,n), &
        work(lwork), iwork(n+6))

!     Read A and B from data file

      Read (nin, *)(a(i,1:n), i=1, n)
      Read (nin, *)(b(i,1:n), i=1, n)

!     Calculate the left and right generalized eigenvectors of the
!     pair (A,B). Note that DTGEVC requires WORK to be of dimension
!     at least 6*n.

      Call dtgevc('Both', 'All', select, n, a, lda, b, ldb, vl, ldvl, vr, &
        ldvr, n, m, work, info)

      If (info>0) Then
        Write (nout, 100) info, info + 1
      Else

!       Estimate condition numbers for all the generalized eigenvalues
!       and right eigenvectors of the pair (A,B)

        Call dtgsna('Both', 'All', select, n, a, lda, b, ldb, vl, ldvl, vr, &
          ldvr, s, dif, n, m, work, lwork, iwork, info)

!       Print condition numbers of eigenvalues and right eigenvectors

        Write (nout, *) 'S'
        Write (nout, 110) s(1:m)
        Write (nout, *)
        Write (nout, *) 'DIF'
        Write (nout, 110) dif(1:m)

!       Calculate approximate error estimates

!       Compute the 1-norms of A and B and then compute
!       SQRT(snorm**2 + tnorm**2)

        eps = epsilon(1.0E0_dp)
        snorm = dlange('1-norm', n, n, a, lda, work)
        tnorm = dlange('1-norm', n, n, b, ldb, work)
        stnrm = nagf_blas_dpyth(snorm, tnorm)
        Write (nout, *)
        Write (nout, *) 'Approximate error estimates for eigenvalues of (A,B)'
        Write (nout, 110)(eps*stnrm/s(i), i=1, m)
        Write (nout, *)
        Write (nout, *) 'Approximate error estimates for right ', &
          'eigenvectors of (A,B)'
        Write (nout, 110)(eps*stnrm/dif(i), i=1, m)
      End If

100   Format (' The 2-by-2 block (', I5, ':', I5, ') does not have a co', &
        'mplex eigenvalue')
110   Format ((3X,1P,7E11.1))
    End Program
