    Program zgeevx_example

!     ZGEEVX Example Program Text

!     Copyright 2017, Numerical Algorithms Group Ltd. http://www.nag.com

!     .. Use Statements ..
      Use lapack_interfaces, Only: zgeevx
      Use lapack_precision, Only: dp
!     .. Implicit None Statement ..
      Implicit None
!     .. Parameters ..
      Integer, Parameter :: nb = 64, nin = 5, nout = 6
!     .. Local Scalars ..
      Real (Kind=dp) :: abnrm, eps, tol
      Integer :: i, ihi, ilo, info, j, lda, ldvl, ldvr, lwork, n
!     .. Local Arrays ..
      Complex (Kind=dp), Allocatable :: a(:, :), vl(:, :), vr(:, :), w(:), &
        work(:)
      Complex (Kind=dp) :: dummy(1)
      Real (Kind=dp), Allocatable :: rconde(:), rcondv(:), rwork(:), scale(:)
!     .. Intrinsic Procedures ..
      Intrinsic :: epsilon, max, nint, real
!     .. Executable Statements ..
      Write (nout, *) 'ZGEEVX Example Program Results'
!     Skip heading in data file
      Read (nin, *)
      Read (nin, *) n
      lda = n
      ldvl = n
      ldvr = n
      Allocate (a(lda,n), vl(ldvl,n), vr(ldvr,n), w(n), rconde(n), rcondv(n), &
        rwork(2*n), scale(n))

!     Use routine workspace query to get optimal workspace.
      lwork = -1
      Call zgeevx('Balance', 'Vectors (left)', 'Vectors (right)', &
        'Both reciprocal condition numbers', n, a, lda, w, vl, ldvl, vr, ldvr, &
        ilo, ihi, scale, abnrm, rconde, rcondv, dummy, lwork, rwork, info)

!     Make sure that there is enough workspace for block size nb.
      lwork = max((nb+1)*n, nint(real(dummy(1))))
      Allocate (work(lwork))

!     Read the matrix A from data file

      Read (nin, *)(a(i,1:n), i=1, n)

!     Solve the eigenvalue problem

      Call zgeevx('Balance', 'Vectors (left)', 'Vectors (right)', &
        'Both reciprocal condition numbers', n, a, lda, w, vl, ldvl, vr, ldvr, &
        ilo, ihi, scale, abnrm, rconde, rcondv, work, lwork, rwork, info)

      If (info==0) Then

!       Compute the machine precision

        eps = epsilon(1.0E0_dp)
        tol = eps*abnrm

!       Print the eigenvalues and vectors, and associated condition
!       number and bounds

        Write (nout, *)
        Write (nout, *) 'Eigenvalues'
        Write (nout, *)
        Write (nout, *) '         Eigenvalue           rcond    error'

        Do j = 1, n

!         Print information on j-th eigenvalue

          If (rconde(j)>0.0_dp) Then
            If (tol/rconde(j)<10.0_dp*eps) Then
              Write (nout, 100) j, w(j), rconde(j), '-'
            Else
              Write (nout, 110) j, w(j), rconde(j), tol/rconde(j)
            End If
          Else
            Write (nout, 100) j, w(j), rconde(j), 'Inf'
          End If

        End Do

        Write (nout, *)
        Write (nout, *) 'Eigenvectors'
        Write (nout, *)
        Write (nout, *) '         Eigenvector          rcond    error'

        Do j = 1, n

!         Print information on j-th eigenvector

          Write (nout, *)

!         Make first real part component be positive
          If (real(vr(1,j))<0.0_dp) Then
            vr(1:n, j) = -vr(1:n, j)
          End If
          If (rcondv(j)>0.0_dp) Then
            If (tol/rcondv(j)<10.0_dp*eps) Then
              Write (nout, 100) j, vr(1, j), rcondv(j), '-'
            Else
              Write (nout, 110) j, vr(1, j), rcondv(j), tol/rcondv(j)
            End If
          Else
            Write (nout, 100) j, vr(1, j), rcondv(j), 'Inf'
          End If

          Write (nout, 120) vr(2:n, j)

        End Do
        Write (nout, *)
        Write (nout, *) 'Errors below 10*machine precision are not displayed'
      Else
        Write (nout, *)
        Write (nout, 130) 'Failure in ZGEEVX. INFO =', info
      End If

100   Format (1X, I2, 1X, '(', 1P, E11.4, ',', E11.4, ')', 1X, 0P, F7.4, 4X, &
        A)
110   Format (1X, I2, 1X, '(', 1P, E11.4, ',', E11.4, ')', 1X, 0P, F7.4, 1X, &
        1P, E8.1)
120   Format (1X, 3X, '(', 1P, E11.4, ',', E11.4, ')')
130   Format (1X, A, I4)

    End Program
