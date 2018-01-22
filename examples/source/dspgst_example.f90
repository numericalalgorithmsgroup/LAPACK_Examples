    Program dspgst_example

!     DSPGST Example Program Text

!     Copyright 2017, Numerical Algorithms Group Ltd. http://www.nag.com

!     .. Use Statements ..
      Use lapack_interfaces, Only: dpptrf, dspgst, dsptrd, dsterf
      Use lapack_precision, Only: dp
!     .. Implicit None Statement ..
      Implicit None
!     .. Parameters ..
      Integer, Parameter :: nin = 5, nout = 6
!     .. Local Scalars ..
      Integer :: i, info, j, n
      Character (1) :: uplo
!     .. Local Arrays ..
      Real (Kind=dp), Allocatable :: ap(:), bp(:), d(:), e(:), tau(:)
!     .. Executable Statements ..
      Write (nout, *) 'DSPGST Example Program Results'
!     Skip heading in data file
      Read (nin, *)
      Read (nin, *) n

      Allocate (ap(n*(n+1)/2), bp(n*(n+1)/2), d(n), e(n-1), tau(n))

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
      Call dpptrf(uplo, n, bp, info)

      Write (nout, *)
      If (info>0) Then
        Write (nout, *) 'B is not positive definite.'
      Else

!       Reduce the problem to standard form C*y = lambda*y, storing
!       the result in A
        Call dspgst(1, uplo, n, ap, bp, info)

!       Reduce C to tridiagonal form T = (Q**T)*C*Q
        Call dsptrd(uplo, n, ap, d, e, tau, info)

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
