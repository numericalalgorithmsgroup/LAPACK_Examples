    Program dspev_example

!     DSPEV Example Program Text

!     Copyright (c) 2018, Numerical Algorithms Group (NAG Ltd.)
!     For licence see
!       https://github.com/numericalalgorithmsgroup/LAPACK_Examples/blob/master/LICENCE.md

!     .. Use Statements ..
      Use lapack_interfaces, Only: dspev
      Use lapack_precision, Only: dp
!     .. Implicit None Statement ..
      Implicit None
!     .. Parameters ..
      Integer, Parameter :: nin = 5, nout = 6
      Character (1), Parameter :: uplo = 'U'
!     .. Local Scalars ..
      Real (Kind=dp) :: eerrbd, eps
      Integer :: i, info, j, n
!     .. Local Arrays ..
      Real (Kind=dp), Allocatable :: ap(:), w(:), work(:)
      Real (Kind=dp) :: dummy(1, 1)
!     .. Intrinsic Procedures ..
      Intrinsic :: abs, epsilon, max
!     .. Executable Statements ..
      Write (nout, *) 'DSPEV Example Program Results'
      Write (nout, *)
!     Skip heading in data file
      Read (nin, *)
      Read (nin, *) n

      Allocate (ap((n*(n+1))/2), w(n), work(3*n))

!     Read the upper or lower triangular part of the matrix A from
!     data file

      If (uplo=='U') Then
        Read (nin, *)((ap(i+(j*(j-1))/2),j=i,n), i=1, n)
      Else If (uplo=='L') Then
        Read (nin, *)((ap(i+((2*n-j)*(j-1))/2),j=1,i), i=1, n)
      End If

!     Solve the symmetric eigenvalue problem
      Call dspev('No vectors', uplo, n, ap, w, dummy, 1, work, info)

      If (info==0) Then

!       Print solution

        Write (nout, *) 'Eigenvalues'
        Write (nout, 100) w(1:n)

!       Get the machine precision, EPS and compute the approximate
!       error bound for the computed eigenvalues.  Note that for
!       the 2-norm, max( abs(W(i)) ) = norm(A), and since the
!       eigenvalues are returned in ascending order
!       max( abs(W(i)) ) = max( abs(W(1)), abs(W(n)))

        eps = epsilon(1.0E0_dp)
        eerrbd = eps*max(abs(w(1)), abs(w(n)))

!       Print the approximate error bound for the eigenvalues

        Write (nout, *)
        Write (nout, *) 'Error estimate for the eigenvalues'
        Write (nout, 110) eerrbd
      Else
        Write (nout, 120) 'Failure in DSPEV. INFO =', info
      End If

100   Format (3X, (8F8.4))
110   Format (4X, 1P, 6E11.1)
120   Format (1X, A, I4)
    End Program
