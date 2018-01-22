    Program zgbbrd_example

!     ZGBBRD Example Program Text

!     Copyright 2017, Numerical Algorithms Group Ltd. http://www.nag.com

!     .. Use Statements ..
      Use lapack_interfaces, Only: zgbbrd
      Use lapack_precision, Only: dp
!     .. Implicit None Statement ..
      Implicit None
!     .. Parameters ..
      Integer, Parameter :: nin = 5, nout = 6
      Character (1), Parameter :: vect = 'N'
!     .. Local Scalars ..
      Integer :: i, info, j, kl, ku, ldab, ldc, ldpt, ldq, m, n, ncc
!     .. Local Arrays ..
      Complex (Kind=dp), Allocatable :: ab(:, :), c(:, :), pt(:, :), q(:, :), &
        work(:)
      Real (Kind=dp), Allocatable :: d(:), e(:), rwork(:)
!     .. Intrinsic Procedures ..
      Intrinsic :: max, min
!     .. Executable Statements ..
      Write (nout, *) 'ZGBBRD Example Program Results'
!     Skip heading in data file
      Read (nin, *)
      Read (nin, *) m, n, kl, ku, ncc
      ldab = kl + ku + 1
      ldc = m
      ldpt = n
      ldq = m
      Allocate (ab(ldab,n), c(m,ncc), pt(ldpt,n), q(ldq,m), work(m+n), d(n), &
        e(n-1), rwork(m+n))

!     Read A from data file

      Read (nin, *)((ab(ku+1+i-j,j),j=max(i-kl,1),min(i+ku,n)), i=1, m)

!     Reduce A to upper bidiagonal form

      Call zgbbrd(vect, m, n, ncc, kl, ku, ab, ldab, d, e, q, ldq, pt, ldpt, &
        c, ldc, work, rwork, info)

!     Print bidiagonal form

      Write (nout, *)
      Write (nout, *) 'Diagonal'
      Write (nout, 100) d(1:min(m,n))
      Write (nout, *) 'Superdiagonal'
      Write (nout, 100) e(1:min(m,n)-1)

100   Format (1X, 8F9.4)
    End Program
