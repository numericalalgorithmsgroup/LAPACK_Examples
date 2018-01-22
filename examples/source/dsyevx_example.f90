    Program dsyevx_example

!     DSYEVX Example Program Text

!     Copyright 2017, Numerical Algorithms Group Ltd. http://www.nag.com

!     .. Use Statements ..
      Use lapack_example_aux, Only: nagf_file_print_matrix_real_gen
      Use lapack_interfaces, Only: dsyevx
      Use lapack_precision, Only: dp
!     .. Implicit None Statement ..
      Implicit None
!     .. Parameters ..
      Real (Kind=dp), Parameter :: zero = 0.0E+0_dp
      Integer, Parameter :: nb = 64, nin = 5, nout = 6
!     .. Local Scalars ..
      Real (Kind=dp) :: abstol, vl, vu
      Integer :: i, ifail, il, info, iu, lda, ldz, lwork, m, n
!     .. Local Arrays ..
      Real (Kind=dp), Allocatable :: a(:, :), w(:), work(:), z(:, :)
      Real (Kind=dp) :: dummy(1)
      Integer, Allocatable :: iwork(:), jfail(:)
!     .. Intrinsic Procedures ..
      Intrinsic :: max, nint
!     .. Executable Statements ..
      Write (nout, *) 'DSYEVX Example Program Results'
      Write (nout, *)
!     Skip heading in data file
      Read (nin, *)
      Read (nin, *) n
      lda = n
      ldz = n
      m = n
      Allocate (a(lda,n), w(n), z(ldz,m), iwork(5*n), jfail(n))

!     Read the lower and upper bounds of the interval to be searched.
      Read (nin, *) vl, vu

!     Use routine workspace query to get optimal workspace.
      lwork = -1
      Call dsyevx('Vectors', 'Values in range', 'Upper', n, a, lda, vl, vu, &
        il, iu, abstol, m, w, z, ldz, dummy, lwork, iwork, jfail, info)

!     Make sure that there is enough workspace for block size nb.
      lwork = max((nb+3)*n, nint(dummy(1)))
      Allocate (work(lwork))

!     Read the upper triangular part of the matrix A.

      Read (nin, *)(a(i,i:n), i=1, n)

!     Set the absolute error tolerance for eigenvalues.  With ABSTOL
!     set to zero, the default value is used instead

      abstol = zero

!     Solve the symmetric eigenvalue problem
      Call dsyevx('Vectors', 'Values in range', 'Upper', n, a, lda, vl, vu, &
        il, iu, abstol, m, w, z, ldz, work, lwork, iwork, jfail, info)

      If (info>=0) Then

!       Print solution

        Write (nout, 100) 'Number of eigenvalues found =', m
        Write (nout, *)
        Write (nout, *) 'Eigenvalues'
        Write (nout, 110) w(1:m)
        Flush (nout)

!       ifail: behaviour on error exit
!              =0 for hard exit, =1 for quiet-soft, =-1 for noisy-soft
        ifail = 0
        Call nagf_file_print_matrix_real_gen('General', ' ', n, m, z, ldz, &
          'Selected eigenvectors', ifail)

        If (info>0) Then
          Write (nout, 100) 'INFO eigenvectors failed to converge, INFO =', &
            info
          Write (nout, *) 'Indices of eigenvectors that did not converge'
          Write (nout, 120) jfail(1:m)
        End If
      Else
        Write (nout, 100) 'Failure in DSYEVX. INFO =', info
      End If

100   Format (1X, A, I5)
110   Format (3X, (8F8.4))
120   Format (3X, (8I8))
    End Program
