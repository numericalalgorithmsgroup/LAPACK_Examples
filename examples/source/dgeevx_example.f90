    Program dgeevx_example

!     DGEEVX Example Program Text

!     Copyright (c) 2018, Numerical Algorithms Group (NAG Ltd.)
!     For licence see
!       https://github.com/numericalalgorithmsgroup/LAPACK_Examples/blob/master/LICENCE.md

!     .. Use Statements ..
      Use lapack_interfaces, Only: dgeevx
      Use lapack_precision, Only: dp
!     .. Implicit None Statement ..
      Implicit None
!     .. Parameters ..
      Integer, Parameter :: nb = 64, nin = 5, nout = 6
!     .. Local Scalars ..
      Complex (Kind=dp) :: eig
      Real (Kind=dp) :: abnrm, eps, tol
      Integer :: i, ihi, ilo, info, j, k, lda, ldvl, ldvr, lwork, n
      Logical :: pair
!     .. Local Arrays ..
      Real (Kind=dp), Allocatable :: a(:, :), rconde(:), rcondv(:), scale(:), &
        vl(:, :), vr(:, :), wi(:), work(:), wr(:)
      Real (Kind=dp) :: dummy(1)
      Integer, Allocatable :: iwork(:)
!     .. Intrinsic Procedures ..
      Intrinsic :: cmplx, epsilon, max, maxloc, nint
!     .. Executable Statements ..
      Write (nout, *) 'DGEEVX Example Program Results'
!     Skip heading in data file
      Read (nin, *)
      Read (nin, *) n
      lda = n
      ldvl = n
      ldvr = n
      lwork = (2+nb)*n
      Allocate (a(lda,n), rconde(n), rcondv(n), scale(n), vl(ldvl,n), &
        vr(ldvr,n), wi(n), wr(n), iwork(2*n-2))

!     Use routine workspace query to get optimal workspace.
      lwork = -1
      Call dgeevx('Balance', 'Vectors (left)', 'Vectors (right)', &
        'Both reciprocal condition numbers', n, a, lda, wr, wi, vl, ldvl, vr, &
        ldvr, ilo, ihi, scale, abnrm, rconde, rcondv, dummy, lwork, iwork, &
        info)

!     Make sure that there is enough workspace for block size nb.
      lwork = max((nb+2)*n, nint(dummy(1)))
      Allocate (work(lwork))

!     Read the matrix A from data file

      Read (nin, *)(a(i,1:n), i=1, n)

!     Solve the eigenvalue problem
      Call dgeevx('Balance', 'Vectors (left)', 'Vectors (right)', &
        'Both reciprocal condition numbers', n, a, lda, wr, wi, vl, ldvl, vr, &
        ldvr, ilo, ihi, scale, abnrm, rconde, rcondv, work, lwork, iwork, &
        info)

      If (info==0) Then

!       Compute the machine precision
        eps = epsilon(1.0E0_dp)
        tol = eps*abnrm
        pair = .False.

!       Print the eigenvalues and vectors, and associated condition
!       number and bounds

        Write (nout, *)
        Write (nout, *) 'Eigenvalues'
        Write (nout, *)
        Write (nout, *) '         Eigenvalue           rcond    error'

        Do j = 1, n

!         Print information on j-th eigenvalue

          If (wi(j)==0.0_dp) Then
            If (rconde(j)>0.0_dp) Then
              If (tol/rconde(j)<10.0_dp*eps) Then
                Write (nout, 100) j, wr(j), rconde(j), '-'
              Else
                Write (nout, 110) j, wr(j), rconde(j), tol/rconde(j)
              End If
            Else
              Write (nout, 110) j, wr(j), rconde(j), 'Inf'
            End If
          Else
            If (rconde(j)>0.0_dp) Then
              If (tol/rconde(j)<10.0_dp*eps) Then
                Write (nout, 120) j, wr(j), wi(j), rconde(j), '-'
              Else
                Write (nout, 130) j, wr(j), wi(j), rconde(j), tol/rconde(j)
              End If
            Else
              Write (nout, 120) j, wr(j), wi(j), rconde(j), 'Inf'
            End If
          End If
        End Do

        Write (nout, *)
        Write (nout, *) 'Eigenvectors'
        Write (nout, *)
        Write (nout, *) '         Eigenvector          rcond    error'

        Do j = 1, n

!         Print information on j-th eigenvector
          Write (nout, *)

          If (wi(j)==0.0E0_dp) Then
!           Make real eigenvectors have positive first entry
            If (vr(1,j)<0.0_dp) Then
              vr(1:n, j) = -vr(1:n, j)
            End If
            If (rcondv(j)>0.0_dp) Then
              If (tol/rcondv(j)<10.0_dp*eps) Then
                Write (nout, 100) j, vr(1, j), rcondv(j), '-'
              Else
                Write (nout, 110) j, vr(1, j), rcondv(j), tol/rcondv(j)
              End If
            Else
              Write (nout, 110) j, vr(1, j), rcondv(j), 'Inf'
            End If
            Write (nout, 140) vr(2:n, j)
          Else
            If (pair) Then
              eig = cmplx(vr(1,j-1), -vr(1,j), kind=dp)
            Else
!             Make largest eigenvector element have positive first entry
              work(1:n) = vr(1:n, j)**2 + vr(1:n, j+1)**2
              k = maxloc(work(1:n), 1)
              If (vr(k,j)<0.0_dp) Then
                vr(1:n, j) = -vr(1:n, j)
              End If
              eig = cmplx(vr(1,j), vr(1,j+1), kind=dp)
            End If
            If (rcondv(j)>0.0_dp) Then
              If (tol/rcondv(j)<10.0_dp*eps) Then
                Write (nout, 120) j, eig, rcondv(j), '-'
              Else
                Write (nout, 130) j, eig, rcondv(j), tol/rcondv(j)
              End If
            Else
              Write (nout, 120) j, eig, rcondv(j), 'Inf'
            End If
            If (pair) Then
              Write (nout, 150)(vr(i,j-1), -vr(i,j), i=2, n)
            Else
              Write (nout, 150)(vr(i,j), vr(i,j+1), i=2, n)
            End If
            pair = .Not. pair
          End If
        End Do
        Write (nout, *)
        Write (nout, *) 'Errors below 10*machine precision are not displayed'
      Else
        Write (nout, *)
        Write (nout, 160) 'Failure in DGEEVX.  INFO = ', info
      End If

100   Format (1X, I2, 2X, 1P, E11.4, 14X, 0P, F7.4, 4X, A)
110   Format (1X, I2, 2X, 1P, E11.4, 11X, 0P, F7.4, 1X, 1P, E8.1)
120   Format (1X, I2, 1X, '(', 1P, E11.4, ',', E11.4, ')', 1X, 0P, F7.4, 4X, &
        A)
130   Format (1X, I2, 1X, '(', 1P, E11.4, ',', E11.4, ')', 1X, 0P, F7.4, 1X, &
        1P, E8.1)
140   Format (1X, 4X, 1P, E11.4)
150   Format (1X, 3X, '(', 1P, E11.4, ',', E11.4, ')')
160   Format (1X, A, I4)
    End Program
