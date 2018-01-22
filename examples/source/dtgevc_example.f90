!   DTGEVC Example Program Text
!   Copyright 2017, Numerical Algorithms Group Ltd. http://www.nag.com

    Module dtgevc_example_mod

!     DTGEVC Example Program Module:
!            Parameters and User-defined Routines

!     .. Use Statements ..
      Use lapack_precision, Only: dp
!     .. Implicit None Statement ..
      Implicit None
!     .. Accessibility Statements ..
      Private
      Public :: normalize
!     .. Parameters ..
      Real (Kind=dp), Parameter, Public :: one = 1.0_dp
      Real (Kind=dp), Parameter, Public :: zero = 0.0_dp
      Integer, Parameter, Public :: nin = 5, nout = 6
    Contains
      Subroutine normalize(n, alphai, v, ldv)

!       .. Use Statements ..
        Use blas_interfaces, Only: dnrm2
!       .. Implicit None Statement ..
        Implicit None
!       .. Scalar Arguments ..
        Integer, Intent (In) :: ldv, n
!       .. Array Arguments ..
        Real (Kind=dp), Intent (In) :: alphai(n)
        Real (Kind=dp), Intent (Inout) :: v(ldv, *)
!       .. Local Scalars ..
        Real (Kind=dp) :: a, b, r, r1, r2, v1, v2
        Integer :: i, j, k
!       .. Intrinsic Procedures ..
        Intrinsic :: sqrt
!       .. Executable Statements ..

        Do j = 1, n

          If (alphai(j)>=0.0_dp) Then
            If (alphai(j)==0.0_dp) Then
!             Real eigenvalue
!             The 2-norm of Q is calculated using dnrm2.
              r = dnrm2(n, v(1,j), 1)
              v(1:n, j) = v(1:n, j)/r
            Else
!             Complex eigenvalue (positive imaginary part)
!             Make largest element real and positive
              r1 = dnrm2(n, v(1,j), 1)
              r2 = dnrm2(n, v(1,j+1), 1)
              r1 = sqrt(r1**2+r2**2)
              r2 = -1.0_dp
              Do i = 1, n
                r = v(i, j)**2 + v(i, j+1)**2
                If (r>r2) Then
                  r2 = r
                  k = i
                End If
              End Do
              r = r1*sqrt(r2)
              a = v(k, j)/r
              b = v(k, j+1)/r
              Do i = 1, n
                v1 = v(i, j)
                v2 = v(i, j+1)
                v(i, j) = v1*a + v2*b
                v(i, j+1) = v2*a - v1*b
              End Do
            End If
          End If
        End Do
      End Subroutine
    End Module
    Program dtgevc_example

!     DTGEVC Example Main Program

!     .. Use Statements ..
      Use dtgevc_example_mod, Only: nin, normalize, nout, one, zero
      Use lapack_example_aux, Only: nagf_file_print_matrix_real_gen_comp
      Use lapack_interfaces, Only: dgeqrf, dggbak, dggbal, dgghrd, dhgeqz, &
        dlacpy, dlaset, dorgqr, dormqr, dtgevc
      Use lapack_precision, Only: dp
!     .. Implicit None Statement ..
      Implicit None
!     .. Local Scalars ..
      Integer :: i, icols, ifail, ihi, ilo, info, irows, jwork, lda, ldb, &
        ldvl, ldvr, lwork, m, n
      Logical :: ileft, iright
      Character (1) :: compq, compz, howmny, job, side
!     .. Local Arrays ..
      Real (Kind=dp), Allocatable :: a(:, :), alphai(:), alphar(:), b(:, :), &
        beta(:), lscale(:), rscale(:), tau(:), vl(:, :), vr(:, :), work(:)
      Logical, Allocatable :: select(:)
      Character (0) :: clabs(1), rlabs(1)
!     .. Intrinsic Procedures ..
      Intrinsic :: nint
!     .. Executable Statements ..
      Write (nout, *) 'DTGEVC Example Program Results'
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
      Allocate (a(lda,n), alphai(n), alphar(n), b(ldb,n), beta(n), lscale(n), &
        rscale(n), tau(n), vl(ldvl,ldvl), vr(ldvr,ldvr), work(lwork), &
        select(n))

!     READ matrix A from data file
      Read (nin, *)(a(i,1:n), i=1, n)

!     READ matrix B from data file
      Read (nin, *)(b(i,1:n), i=1, n)

!     Balance matrix pair (A,B)
      job = 'B'
      Call dggbal(job, n, a, lda, b, ldb, ilo, ihi, lscale, rscale, work, &
        info)

!     Matrix A after balancing
!     ifail: behaviour on error exit
!             =0 for hard exit, =1 for quiet-soft, =-1 for noisy-soft
      ifail = 0
      Call nagf_file_print_matrix_real_gen_comp('General', ' ', n, n, a, lda, &
        'F8.4', 'Matrix A after balancing', 'I', rlabs, 'I', clabs, 80, 0, &
        ifail)
      Write (nout, *)
      Flush (nout)

!     Matrix B after balancing
      ifail = 0
      Call nagf_file_print_matrix_real_gen_comp('General', ' ', n, n, b, ldb, &
        'F8.4', 'Matrix B after balancing', 'I', rlabs, 'I', clabs, 80, 0, &
        ifail)
      Write (nout, *)
      Flush (nout)

!     Reduce B to triangular form using QR
      irows = ihi + 1 - ilo
      icols = n + 1 - ilo
      Call dgeqrf(irows, icols, b(ilo,ilo), ldb, tau, work, lwork, info)

!     Apply the orthogonal transformation to matrix A
      Call dormqr('L', 'T', irows, icols, irows, b(ilo,ilo), ldb, tau, &
        a(ilo,ilo), lda, work, lwork, info)

!     Initialize VL (if left eigenvectors are required)
      If (ileft) Then

        Call dlaset('General', n, n, zero, one, vl, ldvl)
        Call dlacpy('Lower', irows-1, irows-1, b(ilo+1,ilo), ldb, &
          vl(ilo+1,ilo), ldvl)

        Call dorgqr(irows, irows, irows, vl(ilo,ilo), ldvl, tau, work, lwork, &
          info)
      End If

!     Initialize VR (if right eigenvectors are required)
      If (iright) Then
        Call dlaset('General', n, n, zero, one, vr, ldvr)
      End If

!     Compute the generalized Hessenberg form of (A,B)
      compq = 'V'
      compz = 'V'
      Call dgghrd(compq, compz, n, ilo, ihi, a, lda, b, ldb, vl, ldvl, vr, &
        ldvr, info)

!     Matrix A in generalized Hessenberg form
      ifail = 0
      Call nagf_file_print_matrix_real_gen_comp('General', ' ', n, n, a, lda, &
        'F8.4', 'Matrix A in Hessenberg form', 'I', rlabs, 'I', clabs, 80, 0, &
        ifail)
      Write (nout, *)
      Flush (nout)

!     Matrix B in generalized Hessenberg form
      ifail = 0
      Call nagf_file_print_matrix_real_gen_comp('General', ' ', n, n, b, ldb, &
        'F8.4', 'Matrix B in Hessenberg form', 'I', rlabs, 'I', clabs, 80, 0, &
        ifail)

!     Routine DHGEQZ
!     Workspace query: jwork = -1

      jwork = -1
      job = 'S'

      Call dhgeqz(job, compq, compz, n, ilo, ihi, a, lda, b, ldb, alphar, &
        alphai, beta, vl, ldvl, vr, ldvr, work, jwork, info)

      Write (nout, *)
      Write (nout, 100) nint(work(1))
      Write (nout, 110) lwork
      Write (nout, *)
      Write (nout, 120)
      Write (nout, 130)

!     Compute the generalized Schur form
!     if the workspace lwork is adequate
!     The Schur form also gives parameters
!     required to compute generalized eigenvalues

      If (nint(work(1))<=lwork) Then

        Call dhgeqz(job, compq, compz, n, ilo, ihi, a, lda, b, ldb, alphar, &
          alphai, beta, vl, ldvl, vr, ldvr, work, lwork, info)

!       Print the generalized eigenvalues

        Do i = 1, n
          If (beta(i)/=0.0E0_dp) Then
            Write (nout, 140) i, '(', alphar(i)/beta(i), ',', &
              alphai(i)/beta(i), ')'
          Else
            Write (nout, 130) i
          End If
        End Do
        Write (nout, *)
        Flush (nout)

!       Compute left and right generalized eigenvectors
!       of the balanced matrix

        howmny = 'B'
        If (ileft .And. iright) Then
          side = 'B'
        Else If (ileft) Then
          side = 'L'
        Else If (iright) Then
          side = 'R'
        End If

        Call dtgevc(side, howmny, select, n, a, lda, b, ldb, vl, ldvl, vr, &
          ldvr, n, m, work, info)

        If (iright) Then

!         Compute right eigenvectors of the original matrix

          job = 'B'
          side = 'R'

          Call dggbak(job, side, n, ilo, ihi, lscale, rscale, n, vr, ldvr, &
            info)

          Call normalize(n, alphai, vr, ldvr)
!         Print the right eigenvectors

          ifail = 0
          Call nagf_file_print_matrix_real_gen_comp('General', ' ', n, n, vr, &
            ldvr, 'F8.4', 'Right eigenvectors', 'I', rlabs, 'I', clabs, 80, 0, &
            ifail)

          Write (nout, *)
          Flush (nout)
        End If

!       Compute left eigenvectors of the original matrix

        If (ileft) Then
          job = 'B'
          side = 'L'

          Call dggbak(job, side, n, ilo, ihi, lscale, rscale, n, vl, ldvl, &
            info)

          Call normalize(n, alphai, vl, ldvl)
!         Print the left eigenvectors

          ifail = 0
          Call nagf_file_print_matrix_real_gen_comp('General', ' ', n, n, vl, &
            ldvl, 'F8.4', 'Left eigenvectors', 'I', rlabs, 'I', clabs, 80, 0, &
            ifail)

        End If
      Else
        Write (nout, 150)
      End If

100   Format (1X, 'Minimal required LWORK = ', I6)
110   Format (1X, 'Actual value of  LWORK = ', I6)
120   Format (1X, 'Generalized eigenvalues')
130   Format (1X, I4, 5X, 'Infinite eigenvalue')
140   Format (1X, I4, 5X, A, F7.3, A, F7.3, A)
150   Format (1X, 'Insufficient workspace for array WORK', /, ' in DHGEQZ/', &
        'DHGEQZ')
    End Program
