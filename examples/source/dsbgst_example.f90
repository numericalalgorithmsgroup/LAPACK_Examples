    Program dsbgst_example

!     DSBGST Example Program Text

!     Copyright (c) 2018, Numerical Algorithms Group (NAG Ltd.)
!     For licence see
!       https://github.com/numericalalgorithmsgroup/LAPACK_Examples/blob/master/LICENCE.md

!     .. Use Statements ..
      Use lapack_interfaces, Only: dpbstf, dsbgst, dsbtrd, dsterf
      Use lapack_precision, Only: dp
!     .. Implicit None Statement ..
      Implicit None
!     .. Parameters ..
      Integer, Parameter :: nin = 5, nout = 6
!     .. Local Scalars ..
      Integer :: i, info, j, ka, kb, ldab, ldbb, ldx, n
      Character (1) :: uplo
!     .. Local Arrays ..
      Real (Kind=dp), Allocatable :: ab(:, :), bb(:, :), d(:), e(:), work(:), &
        x(:, :)
!     .. Intrinsic Procedures ..
      Intrinsic :: max, min
!     .. Executable Statements ..
      Write (nout, *) 'DSBGST Example Program Results'
!     Skip heading in data file
      Read (nin, *)
      Read (nin, *) n, ka, kb
      ldab = ka + 1
      ldbb = kb + 1
      ldx = n
      Allocate (ab(ldab,n), bb(ldbb,n), d(n), e(n-1), work(2*n), x(ldx,n))

!     Read A and B from data file

      Read (nin, *) uplo
      If (uplo=='U') Then
        Do i = 1, n
          Read (nin, *)(ab(ka+1+i-j,j), j=i, min(n,i+ka))
        End Do
        Do i = 1, n
          Read (nin, *)(bb(kb+1+i-j,j), j=i, min(n,i+kb))
        End Do
      Else If (uplo=='L') Then
        Do i = 1, n
          Read (nin, *)(ab(1+i-j,j), j=max(1,i-ka), i)
        End Do
        Do i = 1, n
          Read (nin, *)(bb(1+i-j,j), j=max(1,i-kb), i)
        End Do
      End If

!     Compute the split Cholesky factorization of B
      Call dpbstf(uplo, n, kb, bb, ldbb, info)

      Write (nout, *)
      If (info>0) Then
        Write (nout, *) 'B is not positive definite.'
      Else

!       Reduce the problem to standard form C*y = lambda*y, storing
!       the result in A
        Call dsbgst('N', uplo, n, ka, kb, ab, ldab, bb, ldbb, x, ldx, work, &
          info)

!       Reduce C to tridiagonal form T = (Q**T)*C*Q
        Call dsbtrd('N', uplo, n, ka, ab, ldab, d, e, x, ldx, work, info)

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

100   Format (3X, (8F8.4))
    End Program
