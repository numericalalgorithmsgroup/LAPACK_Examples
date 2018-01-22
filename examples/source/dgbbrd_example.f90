    Program dgbbrd_example

!     DGBBRD Example Program Text

!     Copyright 2017, Numerical Algorithms Group Ltd. http://www.nag.com

!     .. Use Statements ..
      Use lapack_interfaces, Only: dgbbrd
      Use lapack_precision, Only: dp
!     .. Implicit None Statement ..
      Implicit None
!     .. Parameters ..
      Integer, Parameter :: nin = 5, nout = 6
      Character (1), Parameter :: vect = 'B'
!     .. Local Scalars ..
      Integer :: i, info, j, kl, ku, ldab, ldb, ldc, ldpt, ldq, m, n, ncc
!     .. Local Arrays ..
      Real (Kind=dp), Allocatable :: ab(:, :), b(:, :), c(:, :), d(:), e(:), &
        pt(:, :), q(:, :), work(:)
!     .. Intrinsic Procedures ..
      Intrinsic :: abs, max, min
!     .. Executable Statements ..
      Write (nout, *) 'DGBBRD Example Program Results'
!     Skip heading in data file
      Read (nin, *)
      Read (nin, *) m, n, kl, ku, ncc
      ldab = kl + ku + 1
      ldb = m
      ldc = m
      ldpt = n
      ldq = m
      Allocate (ab(ldab,n), b(ldb,n), c(m,ncc), d(n), e(n-1), pt(ldpt,n), &
        q(ldq,m), work(2*m+2*n))

!     Read A from data file

      Read (nin, *)((ab(ku+1+i-j,j),j=max(i-kl,1),min(i+ku,n)), i=1, m)

!     Reduce A to upper bidiagonal form
      Call dgbbrd(vect, m, n, ncc, kl, ku, ab, ldab, d, e, q, ldq, pt, ldpt, &
        c, ldc, work, info)

!     Print the absolute values of bidiagonal vectors d and e.
!     Any of these can differ by a sign change by combinations of sign
!     changes in columns of Q and P (rows of PT).
      Write (nout, *)
      Write (nout, *) 'Diagonal D:'
      Write (nout, 100) abs(d(1:n))
      Write (nout, *)
      Write (nout, *) 'Off-diagonal E:'
      Write (nout, 100) abs(e(1:n-1))
100   Format (1X, 4(3X,F11.4))

    End Program
