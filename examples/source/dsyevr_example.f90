    Program dsyevr_example

!     DSYEVR Example Program Text

!     Copyright 2017, Numerical Algorithms Group Ltd. http://www.nag.com

!     .. Use Statements ..
      Use lapack_example_aux, Only: nagf_file_print_matrix_real_gen
      Use lapack_interfaces, Only: dsyevr
      Use lapack_precision, Only: dp
!     .. Implicit None Statement ..
      Implicit None
!     .. Parameters ..
      Real (Kind=dp), Parameter :: zero = 0.0E+0_dp
      Integer, Parameter :: nb = 64, nin = 5, nout = 6
!     .. Local Scalars ..
      Real (Kind=dp) :: abstol, vl, vu
      Integer :: i, ifail, il, info, iu, lda, ldz, liwork, lwork, m, n
!     .. Local Arrays ..
      Real (Kind=dp), Allocatable :: a(:, :), w(:), work(:), z(:, :)
      Real (Kind=dp) :: dummy(1)
      Integer :: idum(1)
      Integer, Allocatable :: isuppz(:), iwork(:)
!     .. Intrinsic Procedures ..
      Intrinsic :: max, nint
!     .. Executable Statements ..
      Write (nout, *) 'DSYEVR Example Program Results'
      Write (nout, *)
!     Skip heading in data file and read N and the lower and upper
!     indices of the smallest and largest eigenvalues to be found
      Read (nin, *)
      Read (nin, *) n, il, iu
      lda = n
      ldz = n
      m = n
      Allocate (a(lda,n), w(n), z(ldz,m), isuppz(2*m))

!     Use routine workspace query to get optimal workspace.
      lwork = -1
      liwork = -1
      Call dsyevr('Vectors', 'I', 'Upper', n, a, lda, vl, vu, il, iu, abstol, &
        m, w, z, ldz, isuppz, dummy, lwork, idum, liwork, info)

!     Make sure that there is enough workspace for block size nb.
      lwork = max((nb+6)*n, nint(dummy(1)))
      liwork = max(10*n, idum(1))
      Allocate (work(lwork), iwork(liwork))

!     Read the upper triangular part of the matrix A from data file

      Read (nin, *)(a(i,i:n), i=1, n)

!     Set the absolute error tolerance for eigenvalues.  With ABSTOL
!     set to zero, the default value is used instead

      abstol = zero

!     Solve the symmetric eigenvalue problem
      Call dsyevr('Vectors', 'I', 'Upper', n, a, lda, vl, vu, il, iu, abstol, &
        m, w, z, ldz, isuppz, work, lwork, iwork, liwork, info)

      If (info==0) Then

!       Print solution

        Write (nout, *) 'Selected eigenvalues'
        Write (nout, 100) w(1:m)
        Flush (nout)

!       ifail: behaviour on error exit
!              =0 for hard exit, =1 for quiet-soft, =-1 for noisy-soft
        ifail = 0
        Call nagf_file_print_matrix_real_gen('General', ' ', n, m, z, ldz, &
          'Selected eigenvectors', ifail)

      Else
        Write (nout, 110) 'Failure in DSYEVR. INFO =', info
      End If

100   Format (3X, (8F8.4))
110   Format (1X, A, I5)
    End Program
