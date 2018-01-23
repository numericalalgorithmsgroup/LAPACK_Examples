    Program zheevx_example

!     ZHEEVX Example Program Text

!     Copyright (c) 2018, Numerical Algorithms Group (NAG Ltd.)
!     For licence see
!       https://github.com/numericalalgorithmsgroup/LAPACK_Examples/blob/master/LICENCE.md

!     .. Use Statements ..
      Use blas_interfaces, Only: zscal
      Use lapack_example_aux, Only: nagf_file_print_matrix_complex_gen
      Use lapack_interfaces, Only: zheevx
      Use lapack_precision, Only: dp
!     .. Implicit None Statement ..
      Implicit None
!     .. Parameters ..
      Real (Kind=dp), Parameter :: zero = 0.0E+0_dp
      Integer, Parameter :: nb = 64, nin = 5, nout = 6
!     .. Local Scalars ..
      Real (Kind=dp) :: abstol, vl, vu
      Integer :: i, ifail, il, info, iu, k, lda, ldz, lwork, m, n
!     .. Local Arrays ..
      Complex (Kind=dp), Allocatable :: a(:, :), work(:), z(:, :)
      Complex (Kind=dp) :: dummy(1)
      Real (Kind=dp), Allocatable :: rwork(:), w(:)
      Integer, Allocatable :: iwork(:), jfail(:)
!     .. Intrinsic Procedures ..
      Intrinsic :: abs, cmplx, conjg, max, maxloc, nint, real
!     .. Executable Statements ..
      Write (nout, *) 'ZHEEVX Example Program Results'
      Write (nout, *)
!     Skip heading in data file
      Read (nin, *)
      Read (nin, *) n
      lda = n
      ldz = n
      m = n
      Allocate (a(lda,n), z(ldz,m), rwork(7*n), w(n), iwork(5*n), jfail(n))

!     Read the lower and upper bounds of the interval to be searched.
      Read (nin, *) vl, vu

!     Use routine workspace query to get optimal workspace.
      lwork = -1
      Call zheevx('Vectors', 'Values in range', 'Upper', n, a, lda, vl, vu, &
        il, iu, abstol, m, w, z, ldz, dummy, lwork, rwork, iwork, jfail, info)

!     Make sure that there is enough workspace for block size nb.
      lwork = max((nb+1)*n, nint(real(dummy(1))))
      Allocate (work(lwork))

!     Read the upper triangular part of the matrix A.

      Read (nin, *)(a(i,i:n), i=1, n)

!     Set the absolute error tolerance for eigenvalues.  With ABSTOL
!     set to zero, the default value is used instead

      abstol = zero

!     Solve the Hermitian eigenvalue problem

      Call zheevx('Vectors', 'Values in range', 'Upper', n, a, lda, vl, vu, &
        il, iu, abstol, m, w, z, ldz, work, lwork, rwork, iwork, jfail, info)

      If (info>=0) Then

!       Print solution

        Write (nout, 100) 'Number of eigenvalues found =', m
        Write (nout, *)
        Write (nout, *) 'Eigenvalues'
        Write (nout, 110) w(1:m)
        Flush (nout)

!       Normalize the eigenvectors so that the element of largest absolute
!       value is real.
        Do i = 1, m
          rwork(1:n) = abs(z(1:n,i))
          k = maxloc(rwork(1:n), 1)
          Call zscal(n, conjg(z(k,i))/cmplx(abs(z(k,i)),kind=dp), z(1,i), 1)
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
        Write (nout, 100) 'Failure in ZHEEVX. INFO =', info
      End If

100   Format (1X, A, I5)
110   Format (3X, (8F8.4))
120   Format (3X, (8I8))
    End Program
