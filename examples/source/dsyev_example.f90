    Program dsyev_example

!     DSYEV Example Program Text

!     Copyright (c) 2018, Numerical Algorithms Group (NAG Ltd.)
!     For licence see
!       https://github.com/numericalalgorithmsgroup/LAPACK_Examples/blob/master/LICENCE.md

!     .. Use Statements ..
      Use lapack_example_aux, Only: nagf_blas_damax_val, &
        nagf_file_print_matrix_real_gen
      Use lapack_interfaces, Only: ddisna, dsyev
      Use lapack_precision, Only: dp
!     .. Implicit None Statement ..
      Implicit None
!     .. Parameters ..
      Real (Kind=dp), Parameter :: zero = 0.0_dp
      Integer, Parameter :: nb = 64, nin = 5, nout = 6
!     .. Local Scalars ..
      Real (Kind=dp) :: eerrbd, eps, r
      Integer :: i, ifail, info, k, lda, lwork, n
!     .. Local Arrays ..
      Real (Kind=dp), Allocatable :: a(:, :), rcondz(:), w(:), work(:), &
        zerrbd(:)
      Real (Kind=dp) :: dummy(1)
!     .. Intrinsic Procedures ..
      Intrinsic :: abs, epsilon, max, nint
!     .. Executable Statements ..
      Write (nout, *) 'DSYEV Example Program Results'
      Write (nout, *)
!     Skip heading in data file
      Read (nin, *)
      Read (nin, *) n
      lda = n
      Allocate (a(lda,n), rcondz(n), w(n), zerrbd(n))

!     Use routine workspace query to get optimal workspace.
      lwork = -1
      Call dsyev('Vectors', 'Upper', n, a, lda, w, dummy, lwork, info)

!     Make sure that there is enough workspace for block size nb.
      lwork = max((nb+2)*n, nint(dummy(1)))
      Allocate (work(lwork))

!     Read the upper triangular part of the matrix A from data file

      Read (nin, *)(a(i,i:n), i=1, n)

!     Solve the symmetric eigenvalue problem
      Call dsyev('Vectors', 'Upper', n, a, lda, w, work, lwork, info)

      If (info==0) Then

!       Print solution

        Write (nout, *) 'Eigenvalues'
        Write (nout, 100) w(1:n)
        Flush (nout)

!       Normalize the eigenvectors: largest element positive
        Do i = 1, n
          Call nagf_blas_damax_val(n, a(1,i), 1, k, r)
          If (a(k,i)<zero) Then
            a(1:n, i) = -a(1:n, i)
          End If
        End Do

!       ifail: behaviour on error exit
!              =0 for hard exit, =1 for quiet-soft, =-1 for noisy-soft
        ifail = 0
        Call nagf_file_print_matrix_real_gen('General', ' ', n, n, a, lda, &
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
        Write (nout, 120) 'Failure in DSYEV. INFO =', info
      End If

100   Format (3X, (8F8.4))
110   Format (4X, 1P, 6E11.1)
120   Format (1X, A, I4)
    End Program
