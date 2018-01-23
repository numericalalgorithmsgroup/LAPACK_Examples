    Program zgebrd_example

!     ZGEBRD Example Program Text

!     Copyright (c) 2018, Numerical Algorithms Group (NAG Ltd.)
!     For licence see
!       https://github.com/numericalalgorithmsgroup/LAPACK_Examples/blob/master/LICENCE.md

!     .. Use Statements ..
      Use lapack_interfaces, Only: zgebrd
      Use lapack_precision, Only: dp
!     .. Implicit None Statement ..
      Implicit None
!     .. Parameters ..
      Integer, Parameter :: nin = 5, nout = 6
!     .. Local Scalars ..
      Integer :: i, info, lda, lwork, m, n
!     .. Local Arrays ..
      Complex (Kind=dp), Allocatable :: a(:, :), taup(:), tauq(:), work(:)
      Real (Kind=dp), Allocatable :: d(:), e(:)
!     .. Intrinsic Procedures ..
      Intrinsic :: min
!     .. Executable Statements ..
      Write (nout, *) 'ZGEBRD Example Program Results'
!     Skip heading in data file
      Read (nin, *)
      Read (nin, *) m, n
      lda = m
      lwork = 64*(m+n)
      Allocate (a(lda,n), taup(n), tauq(n), work(lwork), d(n), e(n-1))

!     Read A from data file

      Read (nin, *)(a(i,1:n), i=1, m)

!     Reduce A to bidiagonal form

      Call zgebrd(m, n, a, lda, d, e, tauq, taup, work, lwork, info)

!     Print bidiagonal form

      Write (nout, *)
      Write (nout, *) 'Diagonal'
      Write (nout, 100) d(1:min(m,n))
      If (m>=n) Then
        Write (nout, *) 'Superdiagonal'
      Else
        Write (nout, *) 'Subdiagonal'
      End If
      Write (nout, 100) e(1:min(m,n)-1)

100   Format (1X, 8F9.4)
    End Program
