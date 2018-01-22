    Program dbdsdc_example

!     DBDSDC Example Program Text

!     Copyright 2017, Numerical Algorithms Group Ltd. http://www.nag.com

!     .. Use Statements ..
      Use lapack_interfaces, Only: dbdsdc
      Use lapack_precision, Only: dp
!     .. Implicit None Statement ..
      Implicit None
!     .. Parameters ..
      Integer, Parameter :: nin = 5, nout = 6
!     .. Local Scalars ..
      Integer :: info, ldb, ldu, ldvt, n
!     .. Local Arrays ..
      Real (Kind=dp), Allocatable :: b(:, :), d(:), e(:), u(:, :), vt(:, :), &
        work(:)
      Real (Kind=dp) :: q(1)
      Integer :: iq(1)
      Integer, Allocatable :: iwork(:)
!     .. Executable Statements ..
      Write (nout, *) 'DBDSDC Example Program Results'
      Write (nout, *)
      Flush (nout)
!     Skip heading in data file
      Read (nin, *)
      Read (nin, *) n
      ldb = n
      ldu = n
      ldvt = n
      Allocate (b(ldb,n), d(n), e(n-1), u(ldu,n), vt(ldvt,n), work(n*(3*n+ &
        4)), iwork(8*n))

!     Read the bidiagonal matrix B from data file, first
!     the diagonal elements, and then the off diagonal elements

      Read (nin, *) d(1:n)
      Read (nin, *) e(1:n-1)

!     Calculate the singular values and left and right singular
!     vectors of B.

      Call dbdsdc('Upper', 'I', n, d, e, u, ldu, vt, ldvt, q, iq, work, iwork, &
        info)

      If (info==0) Then
!       Print the singular values of B.

        Write (nout, *) 'Singular values of B:'
        Write (nout, 100) d(1:n)
      Else
        Write (nout, 110) '** DBDSDC failed with INFO = ', info
      End If

100   Format (1X, 4(3X,F11.4))
110   Format (1X, A, I10)
    End Program
