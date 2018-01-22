    Program dsterf_example

!     DSTERF Example Program Text

!     Copyright 2017, Numerical Algorithms Group Ltd. http://www.nag.com

!     .. Use Statements ..
      Use lapack_interfaces, Only: dsterf
      Use lapack_precision, Only: dp
!     .. Implicit None Statement ..
      Implicit None
!     .. Parameters ..
      Integer, Parameter :: nin = 5, nout = 6
!     .. Local Scalars ..
      Integer :: info, n
!     .. Local Arrays ..
      Real (Kind=dp), Allocatable :: d(:), e(:)
!     .. Executable Statements ..
      Write (nout, *) 'DSTERF Example Program Results'
!     Skip heading in data file
      Read (nin, *)
      Read (nin, *) n

      Allocate (d(n), e(n-1))

!     Read T from data file

      Read (nin, *) d(1:n)
      Read (nin, *) e(1:n-1)

!     Calculate the eigenvalues of T
      Call dsterf(n, d, e, info)

      Write (nout, *)
      If (info>0) Then
        Write (nout, *) 'Failure to converge.'
      Else
        Write (nout, *) 'Eigenvalues'
        Write (nout, 100) d(1:n)
      End If

100   Format (3X, (9F8.4))
    End Program
