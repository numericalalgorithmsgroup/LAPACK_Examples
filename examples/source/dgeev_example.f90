    Program dgeev_example

!     DGEEV Example Program Text
!     Copyright 2017, Numerical Algorithms Group Ltd. http://www.nag.com

!     .. Use Statements ..
      Use lapack_interfaces, Only: dgeev
      Use lapack_precision, Only: dp
!     .. Implicit None Statement ..
      Implicit None
!     .. Parameters ..
      Real (Kind=dp), Parameter :: zero = 0.0_dp
      Integer, Parameter :: nb = 64, nin = 5, nout = 6
!     .. Local Scalars ..
      Complex (Kind=dp) :: eig
      Real (Kind=dp) :: alpha, beta, scale
      Integer :: i, info, j, k, lda, ldvr, lwork, n
!     .. Local Arrays ..
      Real (Kind=dp), Allocatable :: a(:, :), vr(:, :), wi(:), work(:), wr(:)
      Real (Kind=dp) :: dummy(1, 1)
!     .. Intrinsic Procedures ..
      Intrinsic :: cmplx, max, maxloc, nint, sqrt
!     .. Executable Statements ..
      Write (nout, *) 'DGEEV Example Program Results'
!     Skip heading in data file
      Read (nin, *)
      Read (nin, *) n
      lda = n
      ldvr = n
      Allocate (a(lda,n), vr(ldvr,n), wi(n), wr(n))

!     Use routine workspace query to get optimal workspace.
      lwork = -1
      Call dgeev('No left vectors', 'Vectors (right)', n, a, lda, wr, wi, &
        dummy, 1, vr, ldvr, dummy, lwork, info)

!     Make sure that there is enough workspace for block size nb.
      lwork = max((nb+2)*n, nint(dummy(1,1)))
      Allocate (work(lwork))

!     Read the matrix A from data file

      Read (nin, *)(a(i,1:n), i=1, n)

!     Compute the eigenvalues and right eigenvectors of A

      Call dgeev('No left vectors', 'Vectors (right)', n, a, lda, wr, wi, &
        dummy, 1, vr, ldvr, work, lwork, info)

      If (info==0) Then

!       Print solution

        Do j = 1, n
          Write (nout, *)
          If (wi(j)==zero) Then
            Write (nout, 100) 'Eigenvalue(', j, ') = ', wr(j)
          Else
            eig = cmplx(wr(j), wi(j), kind=dp)
            Write (nout, 110) 'Eigenvalue(', j, ') = ', eig
          End If
          Write (nout, *)
          Write (nout, 120) 'Eigenvector(', j, ')'
          If (wi(j)==zero) Then
!           Scale by making largest element positive
            k = maxloc(vr(1:n,j), 1)
            If (vr(k,j)<zero) Then
              vr(1:n, j) = -vr(1:n, j)
            End If
            Write (nout, 130) vr(1:n, j)
          Else If (wi(j)>0.0E0_dp) Then
!           Scale by making largest element real and positive
            work(1:n) = vr(1:n, j)**2 + vr(1:n, j+1)**2
            k = maxloc(work(1:n), 1)
            scale = sqrt(work(k))
            work(1:n) = vr(1:n, j)
            alpha = vr(k, j)/scale
            beta = vr(k, j+1)/scale
            vr(1:n, j) = alpha*work(1:n) + beta*vr(1:n, j+1)
            vr(1:n, j+1) = alpha*vr(1:n, j+1) - beta*work(1:n)
            Write (nout, 140)(vr(i,j), vr(i,j+1), i=1, n)
          Else
            Write (nout, 140)(vr(i,j-1), -vr(i,j), i=1, n)
          End If
        End Do
      Else
        Write (nout, *)
        Write (nout, 150) 'Failure in DGEEV.  INFO = ', info
      End If

100   Format (1X, A, I2, A, 1P, E11.4)
110   Format (1X, A, I2, A, '(', 1P, E11.4, ',', 1P, E11.4, ')')
120   Format (1X, A, I2, A)
130   Format (1X, 1P, E11.4)
140   Format (1X, '(', 1P, E11.4, ',', 1P, E11.4, ')')
150   Format (1X, A, I4)
    End Program
