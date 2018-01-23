    Program zhpevd_example

!     ZHPEVD Example Program Text

!     Copyright (c) 2018, Numerical Algorithms Group (NAG Ltd.)
!     For licence see
!       https://github.com/numericalalgorithmsgroup/LAPACK_Examples/blob/master/LICENCE.md

!     .. Use Statements ..
      Use blas_interfaces, Only: dznrm2
      Use lapack_example_aux, Only: nagf_file_print_matrix_complex_gen
      Use lapack_interfaces, Only: zhpevd
      Use lapack_precision, Only: dp
!     .. Implicit None Statement ..
      Implicit None
!     .. Parameters ..
      Integer, Parameter :: nin = 5, nout = 6
!     .. Local Scalars ..
      Complex (Kind=dp) :: scal
      Integer :: i, ifail, info, j, k, ldz, liwork, lrwork, lwork, n
      Character (1) :: job, uplo
!     .. Local Arrays ..
      Complex (Kind=dp), Allocatable :: ap(:), work(:), z(:, :)
      Real (Kind=dp), Allocatable :: rwork(:), w(:)
      Integer, Allocatable :: iwork(:)
!     .. Intrinsic Procedures ..
      Intrinsic :: abs, conjg, maxloc
!     .. Executable Statements ..
      Write (nout, *) 'ZHPEVD Example Program Results'
!     Skip heading in data file
      Read (nin, *)
      Read (nin, *) n
      ldz = n
      liwork = 5*n + 3
      lrwork = 2*n*n + 5*n + 1
      lwork = 2*n
      Allocate (ap(n*(n+1)/2), work(lwork), z(ldz,n), rwork(lrwork), w(n), &
        iwork(liwork))
      Read (nin, *) uplo

!     Read A from data file

      If (uplo=='U') Then
        Read (nin, *)((ap(i+j*(j-1)/2),j=i,n), i=1, n)
      Else If (uplo=='L') Then
        Read (nin, *)((ap(i+(2*n-j)*(j-1)/2),j=1,i), i=1, n)
      End If

      Read (nin, *) job

!     Calculate all the eigenvalues and eigenvectors of A

      Call zhpevd(job, uplo, n, ap, w, z, ldz, work, lwork, rwork, lrwork, &
        iwork, liwork, info)

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

!       Normalize the eigenvectors, largest element real
        Do i = 1, n
          rwork(1:n) = abs(z(1:n,i))
          k = maxloc(rwork(1:n), 1)
          scal = conjg(z(k,i))/abs(z(k,i))/dznrm2(n, z(1,i), 1)
          z(1:n, i) = z(1:n, i)*scal
        End Do

!       ifail: behaviour on error exit
!              =0 for hard exit, =1 for quiet-soft, =-1 for noisy-soft
        ifail = 0
        Call nagf_file_print_matrix_complex_gen('General', ' ', n, n, z, ldz, &
          'Eigenvectors', ifail)

      End If

100   Format (3X, I5, 5X, 2F8.4)
    End Program
