    Program dsyevd_example

!     DSYEVD Example Program Text

!     Copyright (c) 2018, Numerical Algorithms Group (NAG Ltd.)
!     For licence see
!       https://github.com/numericalalgorithmsgroup/LAPACK_Examples/blob/master/LICENCE.md

!     .. Use Statements ..
      Use lapack_example_aux, Only: nagf_blas_damax_val, &
        nagf_file_print_matrix_real_gen
      Use lapack_interfaces, Only: dsyevd
      Use lapack_precision, Only: dp
!     .. Implicit None Statement ..
      Implicit None
!     .. Parameters ..
      Real (Kind=dp), Parameter :: zero = 0.0_dp
      Integer, Parameter :: nin = 5, nout = 6
!     .. Local Scalars ..
      Real (Kind=dp) :: r
      Integer :: i, ifail, info, k, lda, liwork, lwork, n
      Character (1) :: job, uplo
!     .. Local Arrays ..
      Real (Kind=dp), Allocatable :: a(:, :), w(:), work(:)
      Integer, Allocatable :: iwork(:)
!     .. Executable Statements ..
      Write (nout, *) 'DSYEVD Example Program Results'
!     Skip heading in data file
      Read (nin, *)
      Read (nin, *) n
      lda = n
      liwork = 5*n + 3
      lwork = 2*n*n + 6*n + 1
      Allocate (a(lda,n), w(n), work(lwork), iwork(liwork))

!     Read A from data file

      Read (nin, *) uplo
      If (uplo=='U') Then
        Read (nin, *)(a(i,i:n), i=1, n)
      Else If (uplo=='L') Then
        Read (nin, *)(a(i,1:i), i=1, n)
      End If

      Read (nin, *) job

!     Calculate all the eigenvalues and eigenvectors of A
      Call dsyevd(job, uplo, n, a, lda, w, work, lwork, iwork, liwork, info)

      Write (nout, *)
      If (info>0) Then
        Write (nout, *) 'Failure to converge.'
      Else

!       Print eigenvalues and eigenvectors

        Write (nout, *) 'Eigenvalues'
        Write (nout, 100) w(1:n)
        Write (nout, *)
        Flush (nout)

!       Normalize the eigenvectors: largest element positive
        Do i = 1, n
          Call nagf_blas_damax_val(n, a(1,i), 1, k, r)
          If (a(k,i)<zero) Then
            a(1:n, i) = -a(1:n, i)
          End If
        End Do

!       ifail: behaviour on error exit
!              =0 for hard exit, =1 for quiet-soft, =-1 for noisy-soft
        ifail = 0
        Call nagf_file_print_matrix_real_gen('General', ' ', n, n, a, lda, &
          'Eigenvectors', ifail)

      End If

100   Format (3X, (8F8.4))
    End Program
