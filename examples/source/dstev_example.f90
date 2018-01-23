    Program dstev_example

!     DSTEV Example Program Text

!     Copyright (c) 2018, Numerical Algorithms Group (NAG Ltd.)
!     For licence see
!       https://github.com/numericalalgorithmsgroup/LAPACK_Examples/blob/master/LICENCE.md

!     .. Use Statements ..
      Use lapack_example_aux, Only: nagf_file_print_matrix_real_gen
      Use lapack_interfaces, Only: ddisna, dstev
      Use lapack_precision, Only: dp
!     .. Implicit None Statement ..
      Implicit None
!     .. Parameters ..
      Integer, Parameter :: nin = 5, nout = 6
!     .. Local Scalars ..
      Real (Kind=dp) :: eerrbd, eps
      Integer :: i, ifail, info, ldz, n
!     .. Local Arrays ..
      Real (Kind=dp), Allocatable :: d(:), e(:), rcondz(:), work(:), z(:, :), &
        zerrbd(:)
!     .. Intrinsic Procedures ..
      Intrinsic :: abs, epsilon, max
!     .. Executable Statements ..
      Write (nout, *) 'DSTEV Example Program Results'
      Write (nout, *)
!     Skip heading in data file
      Read (nin, *)
      Read (nin, *) n
      ldz = n
      Allocate (d(n), e(n-1), rcondz(n), work(2*n-2), z(ldz,n), zerrbd(n))

!     Read the diagonal and off-diagonal elements of the matrix A
!     from data file

      Read (nin, *) d(1:n)
      Read (nin, *) e(1:n-1)

!     Solve the symmetric tridiagonal eigenvalue problem
      Call dstev('Vectors', n, d, e, z, ldz, work, info)

      If (info==0) Then

!       Print solution

        Write (nout, *) 'Eigenvalues'
        Write (nout, 100) d(1:n)
        Flush (nout)

!       Standardize the eigenvectors so that first elements are non-negative.
        Do i = 1, n
          If (z(1,i)<0.0_dp) Then
            z(1:n, i) = -z(1:n, i)
          End If
        End Do

!       ifail: behaviour on error exit
!              =0 for hard exit, =1 for quiet-soft, =-1 for noisy-soft
        ifail = 0
        Call nagf_file_print_matrix_real_gen('General', ' ', n, n, z, ldz, &
          'Eigenvectors', ifail)

!       Get the machine precision, EPS and compute the approximate
!       error bound for the computed eigenvalues.  Note that for
!       the 2-norm, max( abs(D(i)) ) = norm(A), and since the
!       eigenvalues are returned in ascending order
!       max( abs(D(i)) ) = max( abs(D(1)), abs(D(n)))

        eps = epsilon(1.0E0_dp)
        eerrbd = eps*max(abs(d(1)), abs(d(n)))

!       Call DDISNA to estimate reciprocal condition
!       numbers for the eigenvectors
        Call ddisna('Eigenvectors', n, n, d, rcondz, info)

!       Compute the error estimates for the eigenvectors

        Do i = 1, n
          zerrbd(i) = eerrbd/rcondz(i)
        End Do

!       Print the approximate error bounds for the eigenvalues
!       and vectors

        Write (nout, *)
        Write (nout, *) 'Error estimate for the eigenvalues'
        Write (nout, 110) eerrbd
        Write (nout, *)
        Write (nout, *) 'Error estimates for the eigenvectors'
        Write (nout, 110) zerrbd(1:n)
      Else
        Write (nout, 120) 'Failure in DSTEV. INFO =', info
      End If

100   Format (3X, (8F8.4))
110   Format (4X, 1P, 6E11.1)
120   Format (1X, A, I4)
    End Program
