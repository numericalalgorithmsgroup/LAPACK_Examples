    Program zhegvd_example

!     ZHEGVD Example Program Text

!     Copyright (c) 2018, Numerical Algorithms Group (NAG Ltd.)
!     For licence see
!       https://github.com/numericalalgorithmsgroup/LAPACK_Examples/blob/master/LICENCE.md

!     .. Use Statements ..
      Use lapack_example_aux, Only: nagf_file_print_matrix_complex_gen
      Use lapack_interfaces, Only: ddisna, zhegvd, zlanhe, ztrcon
      Use lapack_precision, Only: dp
!     .. Implicit None Statement ..
      Implicit None
!     .. Parameters ..
      Real (Kind=dp), Parameter :: one = 1.0E+0_dp
      Integer, Parameter :: nb = 64, nin = 5, nout = 6
!     .. Local Scalars ..
      Complex (Kind=dp) :: scal
      Real (Kind=dp) :: anorm, bnorm, eps, rcond, rcondb, t1, t2
      Integer :: i, ifail, info, k, lda, ldb, liwork, lrwork, lwork, n
!     .. Local Arrays ..
      Complex (Kind=dp), Allocatable :: a(:, :), b(:, :), work(:)
      Complex (Kind=dp) :: cdum(1)
      Real (Kind=dp), Allocatable :: eerbnd(:), rcondz(:), rwork(:), w(:), &
        zerbnd(:)
      Real (Kind=dp) :: rdum(1)
      Integer :: idum(1)
      Integer, Allocatable :: iwork(:)
!     .. Intrinsic Procedures ..
      Intrinsic :: abs, conjg, epsilon, max, maxloc, nint, real
!     .. Executable Statements ..
      Write (nout, *) 'ZHEGVD Example Program Results'
      Write (nout, *)
!     Skip heading in data file
      Read (nin, *)
      Read (nin, *) n
      lda = n
      ldb = n
      Allocate (a(lda,n), b(ldb,n), eerbnd(n), rcondz(n), w(n), zerbnd(n))

!     Use routine workspace query to get optimal workspace.
      lwork = -1
      liwork = -1
      lrwork = -1
      Call zhegvd(2, 'Vectors', 'Upper', n, a, lda, b, ldb, w, cdum, lwork, &
        rdum, lrwork, idum, liwork, info)

!     Make sure that there is enough workspace for block size nb.
      lwork = max((nb+2+n)*n, nint(real(cdum(1))))
      lrwork = max(1+(5+2*n)*n, nint(rdum(1)))
      liwork = max(3+5*n, idum(1))
      Allocate (work(lwork), rwork(lrwork), iwork(liwork))

!     Read the upper triangular parts of the matrices A and B

      Read (nin, *)(a(i,i:n), i=1, n)
      Read (nin, *)(b(i,i:n), i=1, n)

!     Compute the one-norms of the symmetric matrices A and B

      anorm = zlanhe('One norm', 'Upper', n, a, lda, rwork)
      bnorm = zlanhe('One norm', 'Upper', n, b, ldb, rwork)

!     Solve the generalized Hermitian eigenvalue problem
!     A*B*x = lambda*x (itype = 2)
      Call zhegvd(2, 'Vectors', 'Upper', n, a, lda, b, ldb, w, work, lwork, &
        rwork, lrwork, iwork, liwork, info)

      If (info==0) Then

!       Print solution

        Write (nout, *) 'Eigenvalues'
        Write (nout, 100) w(1:n)
        Flush (nout)

!       Normalize the eigenvectors, largest element real
        Do i = 1, n
          rwork(1:n) = abs(a(1:n,i))
          k = maxloc(rwork(1:n), 1)
          scal = conjg(a(k,i))/abs(a(k,i))
          a(1:n, i) = a(1:n, i)*scal
        End Do

!       ifail: behaviour on error exit
!              =0 for hard exit, =1 for quiet-soft, =-1 for noisy-soft
        ifail = 0
        Call nagf_file_print_matrix_complex_gen('General', ' ', n, n, a, lda, &
          'Eigenvectors', ifail)

!       Call ZTRCON to estimate the reciprocal condition
!       number of the Cholesky factor of B.  Note that:
!       cond(B) = 1/rcond**2
        Call ztrcon('One norm', 'Upper', 'Non-unit', n, b, ldb, rcond, work, &
          rwork, info)

!       Print the reciprocal condition number of B

        rcondb = rcond**2
        Write (nout, *)
        Write (nout, *) 'Estimate of reciprocal condition number for B'
        Write (nout, 110) rcondb
        Flush (nout)

!       Get the machine precision, eps, and if rcondb is not less
!       than eps**2, compute error estimates for the eigenvalues and
!       eigenvectors

        eps = epsilon(1.0E0_dp)
        If (rcond>=eps) Then

!         Call DDISNA to estimate reciprocal condition
!         numbers for the eigenvectors of (A*B - lambda*I)

          Call ddisna('Eigenvectors', n, n, w, rcondz, info)

!         Compute the error estimates for the eigenvalues and
!         eigenvectors

          t1 = one/rcond
          t2 = anorm*bnorm
          Do i = 1, n
            eerbnd(i) = (t2+abs(w(i))/rcondb)
            zerbnd(i) = t1*(t2/rcondz(i)+t1)
          End Do

!         Print the approximate error bounds for the eigenvalues
!         and vectors

          Write (nout, *)
          Write (nout, *) 'Error estimates (relative to machine precision)'
          Write (nout, *) 'for the eigenvalues:'
          Write (nout, 110) eerbnd(1:n)
          Write (nout, *)
          Write (nout, *) 'for the eigenvectors:'
          Write (nout, 110) zerbnd(1:n)
        Else
          Write (nout, *)
          Write (nout, *) 'B is very ill-conditioned, error ', &
            'estimates have not been computed'
        End If
      Else If (info>n) Then
        i = info - n
        Write (nout, 120) 'The leading minor of order ', i, &
          ' of B is not positive definite'
      Else
        Write (nout, 130) 'Failure in ZHEGVD. INFO =', info
      End If

100   Format (3X, (6F11.4))
110   Format (4X, 1P, 6E11.1)
120   Format (1X, A, I4, A)
130   Format (1X, A, I4)
    End Program
