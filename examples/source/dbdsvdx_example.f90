    Program dbdsvdx_example

!     DBDSVDX Example Program Text

!     Copyright (c) 2018, Numerical Algorithms Group (NAG Ltd.)
!     For licence see
!       https://github.com/numericalalgorithmsgroup/LAPACK_Examples/blob/master/LICENCE.md

!     .. Use Statements ..
      Use lapack_interfaces, Only: dbdsvdx
      Use lapack_precision, Only: dp
!     .. Implicit None Statement ..
      Implicit None
!     .. Parameters ..
      Integer, Parameter :: nin = 5, nout = 6
!     .. Local Scalars ..
      Real (Kind=dp) :: vl, vu
      Integer :: il, info, iu, ldz, n, ns
      Character (1) :: range
!     .. Local Arrays ..
      Real (Kind=dp), Allocatable :: d(:), e(:), s(:), work(:), z(:, :)
      Integer, Allocatable :: iwork(:)
!     .. Executable Statements ..
      Write (nout, *) 'DBDSVDX Example Program Results'
      Write (nout, *)
      Flush (nout)
!     Skip heading in data file
      Read (nin, *)
      Read (nin, *) n
      ldz = 2*n
      Allocate (d(n), e(n-1), s(n), z(ldz,n+1), work(14*n), iwork(12*n))

!     Read the bidiagonal matrix B from data file, first
!     the diagonal elements, and then the off diagonal elements

      Read (nin, *) d(1:n)
      Read (nin, *) e(1:n-1)

!     Read range for selected singular values
      Read (nin, *) range

      If (range=='I' .Or. range=='i') Then
        Read (nin, *) il, iu
      Else If (range=='V' .Or. range=='v') Then
        Read (nin, *) vl, vu
      End If

!     Calculate the singular values and singular vectors of B.

      Call dbdsvdx('Upper', 'V', range, n, d, e, vl, vu, il, iu, ns, s, z, &
        ldz, work, iwork, info)

      If (info==0) Then
!       Print the singular values of B.

        If (range=='I' .Or. range=='i') Then
          Write (nout, 100) ns, il, iu
        Else If (range=='V' .Or. range=='v') Then
          Write (nout, 110) ns, vl, vu
        End If
        Write (nout, 120) s(1:ns)
      Else
        Write (nout, 130) '** DBDSVDX failed with INFO = ', info
      End If

100   Format (1X, I2, 1X, 'singular values of B in the index range [', I2, &
        ',', I2, ']:')
110   Format (1X, I2, 1X, 'singular values of B in the range [', F7.3, ',', &
        F7.3, ']:')
120   Format (1X, 4(3X,F11.4))
130   Format (1X, A, I10)
    End Program
