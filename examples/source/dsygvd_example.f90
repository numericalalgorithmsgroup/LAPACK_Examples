    Program dsygvd_example

!     DSYGVD Example Program Text

!     Copyright (c) 2018, Numerical Algorithms Group (NAG Ltd.)
!     For licence see
!       https://github.com/numericalalgorithmsgroup/LAPACK_Examples/blob/master/LICENCE.md

!     .. Use Statements ..
      Use lapack_example_aux, Only: nagf_blas_damax_val, &
        nagf_file_print_matrix_real_gen
      Use lapack_interfaces, Only: ddisna, dlansy, dsygvd, dtrcon
      Use lapack_precision, Only: dp
!     .. Implicit None Statement ..
      Implicit None
!     .. Parameters ..
      Real (Kind=dp), Parameter :: one = 1.0E+0_dp
      Real (Kind=dp), Parameter :: zero = 0.0_dp
      Integer, Parameter :: nb = 64, nin = 5, nout = 6
!     .. Local Scalars ..
      Real (Kind=dp) :: anorm, bnorm, eps, r, rcond, rcondb, t1, t2, t3
      Integer :: i, ifail, info, k, lda, ldb, liwork, lwork, n
!     .. Local Arrays ..
      Real (Kind=dp), Allocatable :: a(:, :), b(:, :), eerbnd(:), rcondz(:), &
        w(:), work(:), zerbnd(:)
      Real (Kind=dp) :: dummy(1)
      Integer :: idum(1)
      Integer, Allocatable :: iwork(:)
!     .. Intrinsic Procedures ..
      Intrinsic :: abs, epsilon, max, nint
!     .. Executable Statements ..
      Write (nout, *) 'DSYGVD Example Program Results'
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
      Call dsygvd(2, 'Vectors', 'Upper', n, a, lda, b, ldb, w, dummy, lwork, &
        idum, liwork, info)

!     Make sure that there is enough workspace for block size nb.
      lwork = max(1+(nb+6+2*n)*n, nint(dummy(1)))
      liwork = max(3+5*n, idum(1))
      Allocate (work(lwork), iwork(liwork))

!     Read the upper triangular parts of the matrices A and B

      Read (nin, *)(a(i,i:n), i=1, n)
      Read (nin, *)(b(i,i:n), i=1, n)

!     Compute the one-norms of the symmetric matrices A and B

      anorm = dlansy('One norm', 'Upper', n, a, lda, work)
      bnorm = dlansy('One norm', 'Upper', n, b, ldb, work)

!     Solve the generalized symmetric eigenvalue problem
!     A*B*x = lambda*x (ITYPE = 2)

      Call dsygvd(2, 'Vectors', 'Upper', n, a, lda, b, ldb, w, work, lwork, &
        iwork, liwork, info)

      If (info==0) Then

!       Print solution

        Write (nout, *) 'Eigenvalues'
        Write (nout, 100) w(1:n)
        Flush (nout)

!       Normalize the eigenvectors, largest positive
        Do i = 1, n
          Call nagf_blas_damax_val(n, a(1,i), 1, k, r)
          If (a(k,i)<zero) Then
            a(1:n, i) = -a(1:n, i)
          End If
        End Do

!       ifail: behaviour on error exit
!              =0 for hard exit, =1 for quiet-soft, =-1 for noisy-soft
        ifail = 0
        Call nagf_file_print_matrix_real_gen('General', ' ', n, n, a, lda, &
          'Eigenvectors', ifail)

!       Call DTRCON to estimate the reciprocal condition
!       number of the Cholesky factor of B.  Note that:
!       cond(B) = 1/RCOND**2

        Call dtrcon('One norm', 'Upper', 'Non-unit', n, b, ldb, rcond, work, &
          iwork, info)

!       Print the reciprocal condition number of B

        rcondb = rcond**2
        Write (nout, *)
        Write (nout, *) 'Estimate of reciprocal condition number for B'
        Write (nout, 110) rcondb
        Flush (nout)

!       Get the machine precision, EPS, and if RCONDB is not less
!       than EPS**2, compute error estimates for the eigenvalues and
!       eigenvectors

        eps = epsilon(1.0E0_dp)
        If (rcond>=eps) Then

!         Call DDISNA to estimate reciprocal condition
!         numbers for the eigenvectors of (A*B - lambda*I)

          Call ddisna('Eigenvectors', n, n, w, rcondz, info)

!         Compute the error estimates for the eigenvalues and
!         eigenvectors

          t1 = one/rcond
          t2 = eps*t1
          t3 = anorm*bnorm
          Do i = 1, n
            eerbnd(i) = eps*(t3+abs(w(i))/rcondb)
            zerbnd(i) = t2*(t3/rcondz(i)+t1)
          End Do

!         Print the approximate error bounds for the eigenvalues
!         and vectors

          Write (nout, *)
          Write (nout, *) 'Error estimates for the eigenvalues'
          Write (nout, 110) eerbnd(1:n)
          Write (nout, *)
          Write (nout, *) 'Error estimates for the eigenvectors'
          Write (nout, 110) zerbnd(1:n)
        Else
          Write (nout, *)
          Write (nout, *) 'B is very ill-conditioned, error ', &
            'estimates have not been computed'
        End If
      Else If (info>n .And. info<=2*n) Then
        i = info - n
        Write (nout, 120) 'The leading minor of order ', i, &
          ' of B is not positive definite'
      Else
        Write (nout, 130) 'Failure in DSYGVD. INFO =', info
      End If

100   Format (3X, (6F11.4))
110   Format (4X, 1P, 6E11.1)
120   Format (1X, A, I4, A)
130   Format (1X, A, I4)
    End Program
