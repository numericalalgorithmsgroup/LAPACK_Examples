    Program dstevx_example

!     DSTEVX Example Program Text

!     Copyright 2017, Numerical Algorithms Group Ltd. http://www.nag.com

!     .. Use Statements ..
      Use lapack_example_aux, Only: nagf_file_print_matrix_real_gen
      Use lapack_interfaces, Only: dstevx
      Use lapack_precision, Only: dp
!     .. Implicit None Statement ..
      Implicit None
!     .. Parameters ..
      Integer, Parameter :: nin = 5, nout = 6
!     .. Local Scalars ..
      Real (Kind=dp) :: abstol, vl, vu
      Integer :: ifail, il, info, iu, ldz, m, n
!     .. Local Arrays ..
      Real (Kind=dp), Allocatable :: d(:), e(:), w(:), work(:), z(:, :)
      Integer, Allocatable :: iwork(:), jfail(:)
!     .. Intrinsic Procedures ..
      Intrinsic :: tiny
!     .. Executable Statements ..
      Write (nout, *) 'DSTEVX Example Program Results'
      Write (nout, *)
!     Skip heading in data file
      Read (nin, *)
      Read (nin, *) n
      ldz = n
      m = n
      Allocate (d(n), e(n), w(n), work(5*n), z(ldz,m), iwork(5*n), jfail(n))

!     Read the lower and upper bounds of the interval to be searched,
!     and read the diagonal and off-diagonal elements of the matrix
!     A from data file

      Read (nin, *) vl, vu
      Read (nin, *) d(1:n)
      Read (nin, *) e(1:n-1)

!     Set the absolute error tolerance for eigenvalues.  With ABSTOL
!     set to zero, the default value would be used instead

      abstol = 2.0E0_dp*tiny(1.0E0_dp)

!     Solve the symmetric eigenvalue problem
      Call dstevx('Vectors', 'Values in range', n, d, e, vl, vu, il, iu, &
        abstol, m, w, z, ldz, work, iwork, jfail, info)

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
        Write (nout, 100) 'Failure in DSTEVX. INFO =', info
      End If

100   Format (1X, A, I5)
110   Format (3X, (8F8.4))
120   Format (3X, (8I8))
    End Program
