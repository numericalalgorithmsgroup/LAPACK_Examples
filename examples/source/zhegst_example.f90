    Program zhegst_example

!     ZHEGST Example Program Text

!     Copyright (c) 2018, Numerical Algorithms Group (NAG Ltd.)
!     For licence see
!       https://github.com/numericalalgorithmsgroup/LAPACK_Examples/blob/master/LICENCE.md

!     .. Use Statements ..
      Use lapack_interfaces, Only: dsterf, zhegst, zhetrd, zpotrf
      Use lapack_precision, Only: dp
!     .. Implicit None Statement ..
      Implicit None
!     .. Parameters ..
      Integer, Parameter :: nin = 5, nout = 6
!     .. Local Scalars ..
      Integer :: i, info, lda, ldb, lwork, n
      Character (1) :: uplo
!     .. Local Arrays ..
      Complex (Kind=dp), Allocatable :: a(:, :), b(:, :), tau(:), work(:)
      Real (Kind=dp), Allocatable :: d(:), e(:)
!     .. Executable Statements ..
      Write (nout, *) 'ZHEGST Example Program Results'
!     Skip heading in data file
      Read (nin, *)
      Read (nin, *) n
      lda = n
      ldb = n
      lwork = 64*n
      Allocate (a(lda,n), b(ldb,n), tau(n), work(lwork), d(n), e(n-1))

!     Read A and B from data file

      Read (nin, *) uplo
      If (uplo=='U') Then
        Read (nin, *)(a(i,i:n), i=1, n)
        Read (nin, *)(b(i,i:n), i=1, n)
      Else If (uplo=='L') Then
        Read (nin, *)(a(i,1:i), i=1, n)
        Read (nin, *)(b(i,1:i), i=1, n)
      End If

!     Compute the Cholesky factorization of B
      Call zpotrf(uplo, n, b, ldb, info)

      Write (nout, *)
      If (info>0) Then
        Write (nout, *) 'B is not positive definite.'
      Else

!       Reduce the problem to standard form C*y = lambda*y, storing
!       the result in A
        Call zhegst(1, uplo, n, a, lda, b, ldb, info)

!       Reduce C to tridiagonal form T = (Q**H)*C*Q
        Call zhetrd(uplo, n, a, lda, d, e, tau, work, lwork, info)

!       Calculate the eigenvalues of T (same as C)
        Call dsterf(n, d, e, info)

        If (info>0) Then
          Write (nout, *) 'Failure to converge.'
        Else

!         Print eigenvalues

          Write (nout, *) 'Eigenvalues'
          Write (nout, 100) d(1:n)
        End If
      End If

100   Format (3X, (9F8.4))
    End Program
