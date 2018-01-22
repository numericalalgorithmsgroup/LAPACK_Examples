    Program zhpevx_example

!     ZHPEVX Example Program Text

!     Copyright 2017, Numerical Algorithms Group Ltd. http://www.nag.com

!     .. Use Statements ..
      Use blas_interfaces, Only: dznrm2
      Use lapack_example_aux, Only: nagf_file_print_matrix_complex_gen
      Use lapack_interfaces, Only: zhpevx
      Use lapack_precision, Only: dp
!     .. Implicit None Statement ..
      Implicit None
!     .. Parameters ..
      Real (Kind=dp), Parameter :: zero = 0.0E+0_dp
      Integer, Parameter :: nin = 5, nout = 6
      Character (1), Parameter :: uplo = 'U'
!     .. Local Scalars ..
      Complex (Kind=dp) :: scal
      Real (Kind=dp) :: abstol, vl, vu
      Integer :: i, ifail, il, info, iu, j, k, ldz, m, n
!     .. Local Arrays ..
      Complex (Kind=dp), Allocatable :: ap(:), work(:), z(:, :)
      Real (Kind=dp), Allocatable :: rwork(:), w(:)
      Integer, Allocatable :: iwork(:), jfail(:)
!     .. Intrinsic Procedures ..
      Intrinsic :: abs, conjg, maxloc
!     .. Executable Statements ..
      Write (nout, *) 'ZHPEVX Example Program Results'
      Write (nout, *)
!     Skip heading in data file
      Read (nin, *)
      Read (nin, *) n
      ldz = n
      m = n
      Allocate (ap((n*(n+1))/2), work(2*n), z(ldz,m), rwork(7*n), w(n), iwork( &
        5*n), jfail(n))

!     Read the lower and upper bounds of the interval to be searched,
!     and read the upper or lower triangular part of the matrix A
!     from data file

      Read (nin, *) vl, vu
      If (uplo=='U') Then
        Read (nin, *)((ap(i+(j*(j-1))/2),j=i,n), i=1, n)
      Else If (uplo=='L') Then
        Read (nin, *)((ap(i+((2*n-j)*(j-1))/2),j=1,i), i=1, n)
      End If

!     Set the absolute error tolerance for eigenvalues.  With ABSTOL
!     set to zero, the default value is used instead

      abstol = zero

!     Solve the Hermitian eigenvalue problem

      Call zhpevx('Vectors', 'Values in range', uplo, n, ap, vl, vu, il, iu, &
        abstol, m, w, z, ldz, work, rwork, iwork, jfail, info)

      If (info>=0) Then

!       Print solution

        Write (nout, 100) 'Number of eigenvalues found =', m
        Write (nout, *)
        Write (nout, *) 'Eigenvalues'
        Write (nout, 110) w(1:m)
        Flush (nout)

!       Normalize the eigenvectors, largest element real
        Do i = 1, m
          rwork(1:n) = abs(z(1:n,i))
          k = maxloc(rwork(1:n), 1)
          scal = conjg(z(k,i))/abs(z(k,i))/dznrm2(n, z(1,i), 1)
          z(1:n, i) = z(1:n, i)*scal
        End Do

!       ifail: behaviour on error exit
!              =0 for hard exit, =1 for quiet-soft, =-1 for noisy-soft
        ifail = 0
        Call nagf_file_print_matrix_complex_gen('General', ' ', n, m, z, ldz, &
          'Selected eigenvectors', ifail)

        If (info>0) Then
          Write (nout, 100) 'INFO eigenvectors failed to converge, INFO =', &
            info
          Write (nout, *) 'Indices of eigenvectors that did not converge'
          Write (nout, 120) jfail(1:m)
        End If
      Else
        Write (nout, 100) 'Failure in ZHPEVX. INFO =', info
      End If

100   Format (1X, A, I5)
110   Format (3X, (8F8.4))
120   Format (3X, (8I8))
    End Program
