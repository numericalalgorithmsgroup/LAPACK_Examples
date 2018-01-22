    Program dsygvx_example

!     DSYGVX Example Program Text

!     Copyright 2017, Numerical Algorithms Group Ltd. http://www.nag.com

!     .. Use Statements ..
      Use lapack_example_aux, Only: nagf_blas_damax_val, &
        nagf_file_print_matrix_real_gen
      Use lapack_interfaces, Only: dsygvx
      Use lapack_precision, Only: dp
!     .. Implicit None Statement ..
      Implicit None
!     .. Parameters ..
      Real (Kind=dp), Parameter :: zero = 0.0_dp
      Integer, Parameter :: nb = 64, nin = 5, nout = 6
!     .. Local Scalars ..
      Real (Kind=dp) :: abstol, r, vl, vu
      Integer :: i, ifail, il, info, iu, k, lda, ldb, ldz, lwork, m, n
!     .. Local Arrays ..
      Real (Kind=dp), Allocatable :: a(:, :), b(:, :), w(:), work(:), z(:, :)
      Real (Kind=dp) :: dummy(1)
      Integer, Allocatable :: iwork(:), jfail(:)
!     .. Intrinsic Procedures ..
      Intrinsic :: max, nint
!     .. Executable Statements ..
      Write (nout, *) 'DSYGVX Example Program Results'
      Write (nout, *)
!     Skip heading in data file
      Read (nin, *)
      Read (nin, *) n
      lda = n
      ldb = n
      ldz = n
      m = n
      Allocate (a(lda,n), b(ldb,n), w(n), z(ldz,m), iwork(5*n), jfail(n))

!     Read the lower and upper bounds of the interval to be searched.
      Read (nin, *) vl, vu

!     Use routine workspace query to get optimal workspace.
      lwork = -1
      Call dsygvx(1, 'Vectors', 'Values in range', 'Upper', n, a, lda, b, ldb, &
        vl, vu, il, iu, abstol, m, w, z, ldz, dummy, lwork, iwork, jfail, &
        info)

!     Make sure that there is enough workspace for block size nb.
      lwork = max((nb+3)*n, nint(dummy(1)))
      Allocate (work(lwork))

!     Read the upper triangular parts of the matrices A and B

      Read (nin, *)(a(i,i:n), i=1, n)
      Read (nin, *)(b(i,i:n), i=1, n)

!     Set the absolute error tolerance for eigenvalues.  With ABSTOL
!     set to zero, the default value is used instead

      abstol = zero

!     Solve the generalized symmetric eigenvalue problem
!     A*x = lambda*B*x (ITYPE = 1)

      Call dsygvx(1, 'Vectors', 'Values in range', 'Upper', n, a, lda, b, ldb, &
        vl, vu, il, iu, abstol, m, w, z, ldz, work, lwork, iwork, jfail, info)

      If (info>=0 .And. info<=n) Then

!       Print solution

        Write (nout, 100) 'Number of eigenvalues found =', m
        Write (nout, *)
        Write (nout, *) 'Eigenvalues'
        Write (nout, 110) w(1:m)
        Flush (nout)

!       Normalize the eigenvectors, largest positive
        Do i = 1, m
          Call nagf_blas_damax_val(n, z(1,i), 1, k, r)
          If (z(k,i)<zero) Then
            z(1:n, i) = -z(1:n, i)
          End If
        End Do

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
      Else If (info>n .And. info<=2*n) Then
        i = info - n
        Write (nout, 130) 'The leading minor of order ', i, &
          ' of B is not positive definite'
      Else
        Write (nout, 100) 'Failure in DSYGVX. INFO =', info
      End If

100   Format (1X, A, I5)
110   Format (3X, (8F8.4))
120   Format (3X, (8I8))
130   Format (1X, A, I4, A)
    End Program
