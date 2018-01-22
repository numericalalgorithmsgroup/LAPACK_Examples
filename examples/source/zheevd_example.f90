    Program zheevd_example

!     ZHEEVD Example Program Text

!     Copyright 2017, Numerical Algorithms Group Ltd. http://www.nag.com

!     .. Use Statements ..
      Use blas_interfaces, Only: zscal
      Use lapack_example_aux, Only: nagf_file_print_matrix_complex_gen
      Use lapack_interfaces, Only: zheevd
      Use lapack_precision, Only: dp
!     .. Implicit None Statement ..
      Implicit None
!     .. Parameters ..
      Integer, Parameter :: nin = 5, nout = 6
!     .. Local Scalars ..
      Integer :: i, ifail, info, k, lda, liwork, lrwork, lwork, n
      Character (1) :: job, uplo
!     .. Local Arrays ..
      Complex (Kind=dp), Allocatable :: a(:, :), work(:)
      Real (Kind=dp), Allocatable :: rwork(:), w(:)
      Integer, Allocatable :: iwork(:)
!     .. Intrinsic Procedures ..
      Intrinsic :: abs, cmplx, conjg, maxloc
!     .. Executable Statements ..
      Write (nout, *) 'ZHEEVD Example Program Results'
!     Skip heading in data file
      Read (nin, *)
      Read (nin, *) n
      lda = n
      liwork = 5*n + 3
      lrwork = 2*n*n + 5*n + 1
      lwork = n*(n+2)
      Allocate (a(lda,n), work(lwork), rwork(lrwork), w(n), iwork(liwork))
      Read (nin, *) uplo

!     Read A from data file

      If (uplo=='U') Then
        Read (nin, *)(a(i,i:n), i=1, n)
      Else If (uplo=='L') Then
        Read (nin, *)(a(i,1:i), i=1, n)
      End If

      Read (nin, *) job

!     Calculate all the eigenvalues and eigenvectors of A
      Call zheevd(job, uplo, n, a, lda, w, work, lwork, rwork, lrwork, iwork, &
        liwork, info)

      Write (nout, *)
      If (info>0) Then
        Write (nout, *) 'Failure to converge.'
      Else

!       Print eigenvalues and eigenvectors

        Write (nout, *) 'Eigenvalues'
        Do i = 1, n
          Write (nout, 100) i, w(i)
        End Do
        Write (nout, *)
        Flush (nout)

!       Normalize the eigenvectors so that the element of largest absolute
!       value is real.
        Do i = 1, n
          rwork(1:n) = abs(a(1:n,i))
          k = maxloc(rwork(1:n), 1)
          Call zscal(n, conjg(a(k,i))/cmplx(abs(a(k,i)),kind=dp), a(1,i), 1)
        End Do

!       ifail: behaviour on error exit
!              =0 for hard exit, =1 for quiet-soft, =-1 for noisy-soft
        ifail = 0
        Call nagf_file_print_matrix_complex_gen('General', ' ', n, n, a, lda, &
          'Eigenvectors', ifail)

      End If

100   Format (3X, I5, 5X, 2F8.4)
    End Program
