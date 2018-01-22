    Program dorgtr_example

!     DORGTR Example Program Text

!     Copyright 2017, Numerical Algorithms Group Ltd. http://www.nag.com

!     .. Use Statements ..
      Use lapack_example_aux, Only: nagf_blas_damax_val, &
        nagf_file_print_matrix_real_gen
      Use lapack_interfaces, Only: dlacpy, dorgtr, dsteqr, dsytrd
      Use lapack_precision, Only: dp
!     .. Implicit None Statement ..
      Implicit None
!     .. Parameters ..
      Real (Kind=dp), Parameter :: zero = 0.0_dp
      Integer, Parameter :: nin = 5, nout = 6
!     .. Local Scalars ..
      Real (Kind=dp) :: r
      Integer :: i, ifail, info, k, lda, ldz, lwork, n
      Character (1) :: uplo
!     .. Local Arrays ..
      Real (Kind=dp), Allocatable :: a(:, :), d(:), e(:), tau(:), work(:), &
        z(:, :)
!     .. Executable Statements ..
      Write (nout, *) 'DORGTR Example Program Results'
!     Skip heading in data file
      Read (nin, *)
      Read (nin, *) n
      lda = n
      ldz = n
      lwork = 64*n
      Allocate (a(lda,n), d(n), e(n), tau(n), work(lwork), z(ldz,n))

!     Read A from data file

      Read (nin, *) uplo
      If (uplo=='U') Then
        Read (nin, *)(a(i,i:n), i=1, n)
      Else If (uplo=='L') Then
        Read (nin, *)(a(i,1:i), i=1, n)
      End If

!     Reduce A to tridiagonal form T = (Q**T)*A*Q
      Call dsytrd(uplo, n, a, lda, d, e, tau, work, lwork, info)

!     Copy A into Z
      Call dlacpy(uplo, n, n, a, lda, z, ldz)

!     Form Q explicitly, storing the result in Z
      Call dorgtr(uplo, n, z, ldz, tau, work, lwork, info)

!     Calculate all the eigenvalues and eigenvectors of A
      Call dsteqr('V', n, d, e, z, ldz, work, info)

      Write (nout, *)
      If (info>0) Then
        Write (nout, *) 'Failure to converge.'
      Else

!       Print eigenvalues and eigenvectors

        Write (nout, *) 'Eigenvalues'
        Write (nout, 100) d(1:n)
        Write (nout, *)
        Flush (nout)

!       Normalize the eigenvectors: largest element positive
        Do i = 1, n
          Call nagf_blas_damax_val(n, z(1,i), 1, k, r)
          If (z(k,i)<zero) Then
            z(1:n, i) = -z(1:n, i)
          End If
        End Do

!       ifail: behaviour on error exit
!              =0 for hard exit, =1 for quiet-soft, =-1 for noisy-soft
        ifail = 0
        Call nagf_file_print_matrix_real_gen('General', ' ', n, n, z, ldz, &
          'Eigenvectors', ifail)

      End If

100   Format (3X, (8F8.4))
    End Program
