    Program dstevr_example

!     DSTEVR Example Program Text

!     Copyright 2017, Numerical Algorithms Group Ltd. http://www.nag.com

!     .. Use Statements ..
      Use lapack_example_aux, Only: nagf_file_print_matrix_real_gen
      Use lapack_interfaces, Only: dstevr
      Use lapack_precision, Only: dp
!     .. Implicit None Statement ..
      Implicit None
!     .. Parameters ..
      Real (Kind=dp), Parameter :: zero = 0.0_dp
      Integer, Parameter :: nin = 5, nout = 6
!     .. Local Scalars ..
      Real (Kind=dp) :: abstol, vl, vu
      Integer :: ifail, il, info, iu, ldz, liwork, lwork, m, n
!     .. Local Arrays ..
      Real (Kind=dp), Allocatable :: d(:), e(:), w(:), work(:), z(:, :)
      Real (Kind=dp) :: rdum(1)
      Integer :: idum(1)
      Integer, Allocatable :: isuppz(:), iwork(:)
!     .. Intrinsic Procedures ..
      Intrinsic :: max, nint
!     .. Executable Statements ..
      Write (nout, *) 'DSTEVR Example Program Results'
      Write (nout, *)
!     Skip heading in data file and read N and the lower and upper
!     indices of the eigenvalues to be found
      Read (nin, *)
      Read (nin, *) n, il, iu
      ldz = n
      m = n
      Allocate (d(n), e(n-1), w(n), z(ldz,m), isuppz(2*n))

!     Use routine workspace query to get optimal workspace.
      lwork = -1
      liwork = -1
      Call dstevr('Vectors', 'Indices', n, d, e, vl, vu, il, iu, abstol, m, w, &
        z, ldz, isuppz, rdum, lwork, idum, liwork, info)

!     Make sure that there is enough workspace for block size nb.
      lwork = max(20*n, nint(rdum(1)))
      liwork = max(10*n, idum(1))
      Allocate (work(lwork), iwork(liwork))

!     Read the diagonal and off-diagonal elements of the matrix A
!     from data file

      Read (nin, *) d(1:n)
      Read (nin, *) e(1:n-1)

!     Set the absolute error tolerance for eigenvalues.  With ABSTOL
!     set to zero, the default value is used instead

      abstol = zero

!     Solve the symmetric tridiagonal eigenvalue problem
      Call dstevr('Vectors', 'Indices', n, d, e, vl, vu, il, iu, abstol, m, w, &
        z, ldz, isuppz, work, lwork, iwork, liwork, info)

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
        Write (nout, 110) 'Failure in DSTEVR. INFO =', info
      End If

100   Format (3X, (8F8.4))
110   Format (1X, A, I5)
    End Program
