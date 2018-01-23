    Program zhpgst_example

!     ZHPGST Example Program Text

!     Copyright (c) 2018, Numerical Algorithms Group (NAG Ltd.)
!     For licence see
!       https://github.com/numericalalgorithmsgroup/LAPACK_Examples/blob/master/LICENCE.md

!     .. Use Statements ..
      Use lapack_interfaces, Only: dsterf, zhpgst, zhptrd, zpptrf
      Use lapack_precision, Only: dp
!     .. Implicit None Statement ..
      Implicit None
!     .. Parameters ..
      Integer, Parameter :: nin = 5, nout = 6
!     .. Local Scalars ..
      Integer :: i, info, j, n
      Character (1) :: uplo
!     .. Local Arrays ..
      Complex (Kind=dp), Allocatable :: ap(:), bp(:), tau(:)
      Real (Kind=dp), Allocatable :: d(:), e(:)
!     .. Executable Statements ..
      Write (nout, *) 'ZHPGST Example Program Results'
!     Skip heading in data file
      Read (nin, *)
      Read (nin, *) n

      Allocate (ap(n*(n+1)/2), bp(n*(n+1)/2), tau(n), d(n), e(n-1))

!     Read A and B from data file

      Read (nin, *) uplo
      If (uplo=='U') Then
        Read (nin, *)((ap(i+j*(j-1)/2),j=i,n), i=1, n)
        Read (nin, *)((bp(i+j*(j-1)/2),j=i,n), i=1, n)
      Else If (uplo=='L') Then
        Read (nin, *)((ap(i+(2*n-j)*(j-1)/2),j=1,i), i=1, n)
        Read (nin, *)((bp(i+(2*n-j)*(j-1)/2),j=1,i), i=1, n)
      End If

!     Compute the Cholesky factorization of B
      Call zpptrf(uplo, n, bp, info)

      Write (nout, *)
      If (info>0) Then
        Write (nout, *) 'B is not positive definite.'
      Else

!       Reduce the problem to standard form C*y = lambda*y, storing
!       the result in A
        Call zhpgst(1, uplo, n, ap, bp, info)

!       Reduce C to tridiagonal form T = (Q**H)*C*Q
        Call zhptrd(uplo, n, ap, d, e, tau, info)

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
