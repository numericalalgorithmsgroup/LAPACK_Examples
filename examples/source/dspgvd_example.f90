    Program dspgvd_example

!     DSPGVD Example Program Text

!     Copyright 2017, Numerical Algorithms Group Ltd. http://www.nag.com

!     .. Use Statements ..
      Use lapack_interfaces, Only: dlansp, dspgvd, dtpcon
      Use lapack_precision, Only: dp
!     .. Implicit None Statement ..
      Implicit None
!     .. Parameters ..
      Integer, Parameter :: nin = 5, nout = 6
      Character (1), Parameter :: uplo = 'U'
!     .. Local Scalars ..
      Real (Kind=dp) :: anorm, bnorm, eps, rcond, rcondb, t1
      Integer :: aplen, i, info, j, liwork, lwork, n
!     .. Local Arrays ..
      Real (Kind=dp), Allocatable :: ap(:), bp(:), eerbnd(:), w(:), work(:)
      Real (Kind=dp) :: dummy(1, 1)
      Integer :: idum(1)
      Integer, Allocatable :: iwork(:)
!     .. Intrinsic Procedures ..
      Intrinsic :: abs, epsilon, max, nint
!     .. Executable Statements ..
      Write (nout, *) 'DSPGVD Example Program Results'
      Write (nout, *)
!     Skip heading in data file
      Read (nin, *)
      Read (nin, *) n
      aplen = (n*(n+1))/2
      Allocate (ap(aplen), bp(aplen), eerbnd(n), w(n))

!     Use routine workspace query to get optimal workspace.
      lwork = -1
      liwork = -1
      Call dspgvd(2, 'No vectors', uplo, n, ap, bp, w, dummy, n, dummy, lwork, &
        idum, liwork, info)

!     Make sure that there is at least minimum workspace.
      lwork = max(3*n, nint(dummy(1,1)))
      liwork = max(n, idum(1))
      Allocate (work(lwork), iwork(liwork))

!     Read the upper or lower triangular parts of the matrices A and
!     B from data file

      If (uplo=='U') Then
        Read (nin, *)((ap(i+(j*(j-1))/2),j=i,n), i=1, n)
        Read (nin, *)((bp(i+(j*(j-1))/2),j=i,n), i=1, n)
      Else If (uplo=='L') Then
        Read (nin, *)((ap(i+((2*n-j)*(j-1))/2),j=1,i), i=1, n)
        Read (nin, *)((bp(i+((2*n-j)*(j-1))/2),j=1,i), i=1, n)
      End If

!     Compute the one-norms of the symmetric matrices A and B

      anorm = dlansp('One norm', uplo, n, ap, work)
      bnorm = dlansp('One norm', uplo, n, bp, work)

!     Solve the generalized symmetric eigenvalue problem
!     A*B*x = lambda*x (itype = 2)

!     In the following call the 9th argument is set to n rather
!     than 1 to avoid an incorrect error message in some vendor
!     versions of LAPACK.
      Call dspgvd(2, 'No vectors', uplo, n, ap, bp, w, dummy, n, work, lwork, &
        iwork, liwork, info)

      If (info==0) Then

!       Print solution

        Write (nout, *) 'Eigenvalues'
        Write (nout, 100) w(1:n)

!       Call DTPCON to estimate the reciprocal condition
!       number of the Cholesky factor of B.  Note that:
!       cond(B) = 1/rcond**2.  DTPCON requires WORK and IWORK to be
!       of length at least 3*n and n respectively

        Call dtpcon('One norm', uplo, 'Non-unit', n, bp, rcond, work, iwork, &
          info)

!       Print the reciprocal condition number of B

        rcondb = rcond**2
        Write (nout, *)
        Write (nout, *) 'Estimate of reciprocal condition number for B'
        Write (nout, 110) rcondb

!       Get the machine precision, eps, and if rcondb is not less
!       than eps**2, compute error estimates for the eigenvalues

        eps = epsilon(1.0E0_dp)
        If (rcond>=eps) Then
          t1 = anorm*bnorm
          Do i = 1, n
            eerbnd(i) = eps*(t1+abs(w(i))/rcondb)
          End Do

!         Print the approximate error bounds for the eigenvalues

          Write (nout, *)
          Write (nout, *) 'Error estimates for the eigenvalues'
          Write (nout, 110) eerbnd(1:n)
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
        Write (nout, 130) 'Failure in DSPGVD. INFO =', info
      End If

100   Format (3X, (6F11.4))
110   Format (4X, 1P, 6E11.1)
120   Format (1X, A, I4, A)
130   Format (1X, A, I4)
    End Program
