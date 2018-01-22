    Program dgesdd_example

!     DGESDD Example Program Text

!     Copyright 2017, Numerical Algorithms Group Ltd. http://www.nag.com

!     .. Use Statements ..
      Use lapack_example_aux, Only: nagf_file_print_matrix_real_gen
      Use lapack_interfaces, Only: ddisna, dgesdd
      Use lapack_precision, Only: dp
!     .. Implicit None Statement ..
      Implicit None
!     .. Parameters ..
      Integer, Parameter :: nb = 64, nin = 5, nout = 6
!     .. Local Scalars ..
      Real (Kind=dp) :: eps, serrbd
      Integer :: i, ifail, info, lda, ldu, lwork, m, n
!     .. Local Arrays ..
      Real (Kind=dp), Allocatable :: a(:, :), rcondu(:), rcondv(:), s(:), &
        u(:, :), uerrbd(:), verrbd(:), work(:)
      Real (Kind=dp) :: dummy(1, 1)
      Integer, Allocatable :: iwork(:)
!     .. Intrinsic Procedures ..
      Intrinsic :: epsilon, max, min, nint
!     .. Executable Statements ..
      Write (nout, *) 'DGESDD Example Program Results'
      Write (nout, *)
!     Skip heading in data file
      Read (nin, *)
      Read (nin, *) m, n
      lda = m
      ldu = m
      Allocate (a(lda,n), rcondu(m), rcondv(m), s(m), u(ldu,m), uerrbd(m), &
        verrbd(m), iwork(8*min(m,n)))

!     Use routine workspace query to get optimal workspace.
      lwork = -1
      Call dgesdd('Overwrite A by tranpose(V)', m, n, a, lda, s, u, ldu, &
        dummy, 1, dummy, lwork, iwork, info)

!     Make sure that there is enough workspace for block size nb.
      lwork = max((5*m+9)*m+n+nb*(m+n), nint(dummy(1,1)))
      Allocate (work(lwork))

!     Read the m by n matrix A from data file

      Read (nin, *)(a(i,1:n), i=1, m)

!     Compute the singular values and left and right singular vectors
!     of A (A = U*S*(V**T), m.le.n)

      Call dgesdd('Overwrite A by tranpose(V)', m, n, a, lda, s, u, ldu, &
        dummy, 1, work, lwork, iwork, info)

      If (info==0) Then

!       Print solution

        Write (nout, *) 'Singular values'
        Write (nout, 100) s(1:m)
        Flush (nout)

!       Normalize so that u(1,j)>=0
        Do i = 1, m
          If (u(1,i)<0.0_dp) Then
            u(1:m, i) = -u(1:m, i)
            a(i, 1:n) = -a(i, 1:n)
          End If
        End Do
!       ifail: behaviour on error exit
!              =0 for hard exit, =1 for quiet-soft, =-1 for noisy-soft
        ifail = 0
        Call nagf_file_print_matrix_real_gen('General', ' ', m, m, u, ldu, &
          'Left singular vectors', ifail)

        Write (nout, *)
        Flush (nout)

        Call nagf_file_print_matrix_real_gen('General', ' ', m, n, a, lda, &
          'Right singular vectors by row '//'(first m rows of V**T)', ifail)

!       Get the machine precision, EPS and compute the approximate
!       error bound for the computed singular values.  Note that for
!       the 2-norm, S(1) = norm(A)

        eps = epsilon(1.0E0_dp)
        serrbd = eps*s(1)

!       Call DDISNA to estimate reciprocal condition
!       numbers for the singular vectors

        Call ddisna('Left', m, n, s, rcondu, info)
        Call ddisna('Right', m, n, s, rcondv, info)

!       Compute the error estimates for the singular vectors

        Do i = 1, m
          uerrbd(i) = serrbd/rcondu(i)
          verrbd(i) = serrbd/rcondv(i)
        End Do

!       Print the approximate error bounds for the singular values
!       and vectors

        Write (nout, *)
        Write (nout, *) 'Error estimate for the singular values'
        Write (nout, 110) serrbd
        Write (nout, *)
        Write (nout, *) 'Error estimates for the left singular vectors'
        Write (nout, 110) uerrbd(1:m)
        Write (nout, *)
        Write (nout, *) 'Error estimates for the right singular vectors'
        Write (nout, 110) verrbd(1:m)
      Else
        Write (nout, 120) 'Failure in DGESDD. INFO =', info
      End If

100   Format (3X, (8F8.4))
110   Format (4X, 1P, 6E11.1)
120   Format (1X, A, I4)
    End Program
