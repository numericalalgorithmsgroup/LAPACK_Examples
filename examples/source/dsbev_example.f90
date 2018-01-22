    Program dsbev_example

!     DSBEV Example Program Text

!     Copyright 2017, Numerical Algorithms Group Ltd. http://www.nag.com

!     .. Use Statements ..
      Use lapack_example_aux, Only: nagf_file_print_matrix_real_gen
      Use lapack_interfaces, Only: ddisna, dsbev
      Use lapack_precision, Only: dp
!     .. Implicit None Statement ..
      Implicit None
!     .. Parameters ..
      Integer, Parameter :: nin = 5, nout = 6
      Character (1), Parameter :: uplo = 'U'
!     .. Local Scalars ..
      Real (Kind=dp) :: eerrbd, eps
      Integer :: i, ifail, info, j, kd, ldab, ldz, n
!     .. Local Arrays ..
      Real (Kind=dp), Allocatable :: ab(:, :), rcondz(:), w(:), work(:), &
        z(:, :), zerrbd(:)
!     .. Intrinsic Procedures ..
      Intrinsic :: abs, epsilon, max, min
!     .. Executable Statements ..
      Write (nout, *) 'DSBEV Example Program Results'
      Write (nout, *)
!     Skip heading in data file
      Read (nin, *)
      Read (nin, *) n, kd
      ldab = kd + 1
      ldz = n
      Allocate (ab(ldab,n), rcondz(n), w(n), work(3*n-2), z(ldz,n), zerrbd(n))

!     Read the upper or lower triangular part of the symmetric band
!     matrix A from data file

      If (uplo=='U') Then
        Read (nin, *)((ab(kd+1+i-j,j),j=i,min(n,i+kd)), i=1, n)
      Else If (uplo=='L') Then
        Read (nin, *)((ab(1+i-j,j),j=max(1,i-kd),i), i=1, n)
      End If

!     Solve the band symmetric eigenvalue problem

      Call dsbev('Vectors', uplo, n, kd, ab, ldab, w, z, ldz, work, info)

      If (info==0) Then

!       Print solution

        Write (nout, *) 'Eigenvalues'
        Write (nout, 100) w(1:n)
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
!       the 2-norm, max( abs(W(i)) ) = norm(A), and since the
!       eigenvalues are returned in ascending order
!       max( abs(W(i)) ) = max( abs(W(1)), abs(W(n)))

        eps = epsilon(1.0E0_dp)
        eerrbd = eps*max(abs(w(1)), abs(w(n)))

!       Call DDISNA to estimate reciprocal condition
!       numbers for the eigenvectors
        Call ddisna('Eigenvectors', n, n, w, rcondz, info)

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
        Write (nout, 120) 'Failure in DSBEV. INFO =', info
      End If

100   Format (3X, (8F8.4))
110   Format (4X, 1P, 6E11.1)
120   Format (1X, A, I4)
    End Program
