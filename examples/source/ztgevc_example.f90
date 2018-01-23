    Program ztgevc_example

!     ZTGEVC Example Program Text

!     Copyright (c) 2018, Numerical Algorithms Group (NAG Ltd.)
!     For licence see
!       https://github.com/numericalalgorithmsgroup/LAPACK_Examples/blob/master/LICENCE.md

!     .. Use Statements ..
      Use blas_interfaces, Only: dznrm2
      Use lapack_example_aux, Only: nagf_blas_zmload, &
        nagf_file_print_matrix_complex_gen_comp, &
        nagf_sort_cmplxvec_rank_rearrange, nagf_sort_realvec_rank
      Use lapack_interfaces, Only: zgeqrf, zggbak, zggbal, zgghrd, zhgeqz, &
        zlacpy, ztgevc, zungqr, zunmqr
      Use lapack_precision, Only: dp
!     .. Implicit None Statement ..
      Implicit None
!     .. Parameters ..
      Complex (Kind=dp), Parameter :: cone = (1.0E0_dp, 0.0E0_dp)
      Complex (Kind=dp), Parameter :: czero = (0.0E0_dp, 0.0E0_dp)
      Integer, Parameter :: nin = 5, nout = 6
      Logical, Parameter :: prbal = .False., prhess = .False.
!     .. Local Scalars ..
      Complex (Kind=dp) :: scal
      Integer :: i, icols, ifail, ihi, ilo, info, irows, j, jwork, k, lda, &
        ldb, ldvl, ldvr, lwork, m, n
      Logical :: ileft, iright
      Character (1) :: compq, compz, howmny, job, side
!     .. Local Arrays ..
      Complex (Kind=dp), Allocatable :: a(:, :), alpha(:), b(:, :), beta(:), &
        tau(:), v(:, :), vl(:, :), vr(:, :), work(:), zwork(:)
      Real (Kind=dp), Allocatable :: lscale(:), rscale(:), rwork(:)
      Integer, Allocatable :: irank(:)
      Logical, Allocatable :: select(:)
      Character (1) :: clabs(1), rlabs(1)
!     .. Intrinsic Procedures ..
      Intrinsic :: abs, aimag, all, cmplx, conjg, maxloc, nint, real
!     .. Executable Statements ..
      Write (nout, *) 'ZTGEVC Example Program Results'
      Flush (nout)

!     ileft  is TRUE if left  eigenvectors are required
!     iright is TRUE if right eigenvectors are required

      ileft = .True.
      iright = .True.

!     Skip heading in data file
      Read (nin, *)
      Read (nin, *) n
      lda = n
      ldb = n
      ldvl = n
      ldvr = n
      lwork = 6*n
      Allocate (a(lda,n), alpha(n), b(ldb,n), beta(n), tau(n), vl(ldvl,ldvl), &
        vr(ldvr,ldvr), work(lwork), lscale(n), rscale(n), rwork(6*n), &
        select(n), irank(n), v(n,n))

!     READ matrix A from data file
      Read (nin, *)(a(i,1:n), i=1, n)

!     READ matrix B from data file
      Read (nin, *)(b(i,1:n), i=1, n)

!     Balance matrix pair (A,B)
      job = 'B'
      Call zggbal(job, n, a, lda, b, ldb, ilo, ihi, lscale, rscale, rwork, &
        info)

      If (prbal) Then
!       Matrix A after balancing
!       ifail: behaviour on error exit
!               =0 for hard exit, =1 for quiet-soft, =-1 for noisy-soft
        ifail = 0
        Call nagf_file_print_matrix_complex_gen_comp('General', ' ', n, n, a, &
          lda, 'Bracketed', 'F7.4', 'Matrix A after balancing', 'Integer', &
          rlabs, 'Integer', clabs, 80, 0, ifail)
        Write (nout, *)
        Flush (nout)

!       Matrix B after balancing
        ifail = 0
        Call nagf_file_print_matrix_complex_gen_comp('General', ' ', n, n, b, &
          ldb, 'Bracketed', 'F7.4', 'Matrix B after balancing', 'Integer', &
          rlabs, 'Integer', clabs, 80, 0, ifail)
        Write (nout, *)
        Flush (nout)
      End If

!     Reduce B to triangular form using QR
      irows = ihi + 1 - ilo
      icols = n + 1 - ilo
      Call zgeqrf(irows, icols, b(ilo,ilo), ldb, tau, work, lwork, info)

!     Apply the orthogonal transformation to A
      Call zunmqr('L', 'C', irows, icols, irows, b(ilo,ilo), ldb, tau, &
        a(ilo,ilo), lda, work, lwork, info)

!     Initialize VL (for left eigenvectors)
      If (ileft) Then

        Call nagf_blas_zmload('General', n, n, czero, cone, vl, ldvl)
        Call zlacpy('Lower', irows-1, irows-1, b(ilo+1,ilo), ldb, &
          vl(ilo+1,ilo), ldvl)
        Call zungqr(irows, irows, irows, vl(ilo,ilo), ldvl, tau, work, lwork, &
          info)

      End If

!     Initialize VR for right eigenvectors
      If (iright) Then
        Call nagf_blas_zmload('General', n, n, czero, cone, vr, ldvr)
      End If

!     Compute the generalized Hessenberg form of (A,B)
      compq = 'V'
      compz = 'V'
      Call zgghrd(compq, compz, n, ilo, ihi, a, lda, b, ldb, vl, ldvl, vr, &
        ldvr, info)

      If (prhess) Then
!       Matrix A in generalized Hessenberg form
        ifail = 0
        Call nagf_file_print_matrix_complex_gen_comp('General', ' ', n, n, a, &
          lda, 'Bracketed', 'F7.3', 'Matrix A in Hessenberg form', 'Integer', &
          rlabs, 'Integer', clabs, 80, 0, ifail)
        Write (nout, *)
        Flush (nout)

!       Matrix B in generalized Hessenberg form
        ifail = 0
        Call nagf_file_print_matrix_complex_gen_comp('General', ' ', n, n, b, &
          ldb, 'Bracketed', 'F7.3', 'Matrix B in Hessenberg form', 'Integer', &
          rlabs, 'Integer', clabs, 80, 0, ifail)
        Write (nout, *)
        Flush (nout)
      End If

!     Routine ZHGEQZ
!     Workspace query: jwork = -1
      jwork = -1
      job = 'S'
      Call zhgeqz(job, compq, compz, n, ilo, ihi, a, lda, b, ldb, alpha, beta, &
        vl, ldvl, vr, ldvr, work, jwork, rwork, info)

      lwork = nint(real(work(1)))
      Allocate (zwork(lwork))

!     Compute the generalized Schur form
      Call zhgeqz(job, compq, compz, n, ilo, ihi, a, lda, b, ldb, alpha, beta, &
        vl, ldvl, vr, ldvr, zwork, lwork, rwork, info)

!     Sort and print generalized eigenvalues if none are infinite.
      If (all(real(beta(1:n))>0.0_dp)) Then
!       Store absolute values of eigenvalues for ranking
        work(1:n) = alpha(1:n)/beta(1:n)
        rwork(1:n) = abs(work(1:n))

!       Rank eigenvalues
        ifail = 0
        Call nagf_sort_realvec_rank(rwork, 1, n, 'Descending', irank, ifail)

!       Sort eigenvalues in work(1:n)
        Call nagf_sort_cmplxvec_rank_rearrange(work, 1, n, irank, ifail)

        Write (nout, 100)
        Do i = 1, n
          Write (nout, 110) i, '(', real(work(i)), ',', aimag(work(i)), ')'
        End Do
        Write (nout, *)
        Flush (nout)
      Else
        irank(1:n) = (/ (i,i=1,n) /)
      End If

!     Compute left and right generalized eigenvectors
!     of the balanced matrix
      howmny = 'B'
      If (ileft .And. iright) Then
        side = 'B'
      Else If (ileft) Then
        side = 'L'
      Else If (iright) Then
        side = 'R'
      End If

      Call ztgevc(side, howmny, select, n, a, lda, b, ldb, vl, ldvl, vr, ldvr, &
        n, m, work, rwork, info)

!     Compute right eigenvectors of the original matrix

      If (iright) Then
        job = 'B'
        side = 'R'

        Call zggbak(job, side, n, ilo, ihi, lscale, rscale, n, vr, ldvr, info)

!       Normalize the right eigenvectors
        Do i = 1, n
          j = irank(i)
          rwork(1:n) = abs(vr(1:n,i))
          k = maxloc(rwork(1:n), 1)
          scal = conjg(vr(k,i))/abs(vr(k,i))/dznrm2(n, vr(1,i), 1)
          v(1:n, j) = vr(1:n, i)*scal
          v(k, j) = cmplx(real(v(k,j)), kind=dp)
        End Do

!       Print the right eigenvectors

        ifail = 0
        Call nagf_file_print_matrix_complex_gen_comp('General', ' ', n, n, v, &
          n, 'Bracketed', 'F7.4', 'Right eigenvectors', 'Integer', rlabs, &
          'Integer', clabs, 80, 0, ifail)

        Write (nout, *)
        Flush (nout)

      End If

!     Compute left eigenvectors of the original matrix

      If (ileft) Then
        job = 'B'
        side = 'L'

        Call zggbak(job, side, n, ilo, ihi, lscale, rscale, n, vl, ldvl, info)

!       Normalize the left eigenvectors
        Do i = 1, n
          j = irank(i)
          rwork(1:n) = abs(vl(1:n,i))
          k = maxloc(rwork(1:n), 1)
          scal = conjg(vl(k,i))/abs(vl(k,i))/dznrm2(n, vl(1,i), 1)
          v(1:n, j) = vl(1:n, i)*scal
          v(k, j) = cmplx(real(v(k,j)), kind=dp)
        End Do

!       Print the left eigenvectors

        ifail = 0
        Call nagf_file_print_matrix_complex_gen_comp('General', ' ', n, n, v, &
          n, 'Bracketed', 'F7.4', 'Left eigenvectors', 'Integer', rlabs, &
          'Integer', clabs, 80, 0, ifail)

      End If

100   Format (1X, /, 1X, 'Generalized eigenvalues')
110   Format (1X, I4, 5X, A, F7.3, A, F7.3, A)
    End Program
