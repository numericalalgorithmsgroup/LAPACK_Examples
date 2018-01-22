    Program dgebrd_example

!     DGEBRD Example Program Text

!     Copyright 2017, Numerical Algorithms Group Ltd. http://www.nag.com

!     .. Use Statements ..
      Use lapack_interfaces, Only: dgebrd
      Use lapack_precision, Only: dp
!     .. Implicit None Statement ..
      Implicit None
!     .. Parameters ..
      Integer, Parameter :: nin = 5, nout = 6
!     .. Local Scalars ..
      Integer :: i, info, lda, lwork, m, n
!     .. Local Arrays ..
      Real (Kind=dp), Allocatable :: a(:, :), d(:), e(:), taup(:), tauq(:), &
        work(:)
!     .. Intrinsic Procedures ..
      Intrinsic :: min
!     .. Executable Statements ..
      Write (nout, *) 'DGEBRD Example Program Results'
!     Skip heading in data file
      Read (nin, *)
      Read (nin, *) m, n
      lda = m
      lwork = 64*(m+n)
      Allocate (a(lda,n), d(n), e(n-1), taup(n), tauq(n), work(lwork))

!     Read A from data file

      Read (nin, *)(a(i,1:n), i=1, m)

!     Reduce A to bidiagonal form

      Call dgebrd(m, n, a, lda, d, e, tauq, taup, work, lwork, info)

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
