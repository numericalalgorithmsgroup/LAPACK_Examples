    Program dsyrfs_example

!     DSYRFS Example Program Text

!     Copyright 2017, Numerical Algorithms Group Ltd. http://www.nag.com

!     .. Use Statements ..
      Use lapack_example_aux, Only: nagf_file_print_matrix_real_gen
      Use lapack_interfaces, Only: dsyrfs, dsytrf, dsytrs
      Use lapack_precision, Only: dp
!     .. Implicit None Statement ..
      Implicit None
!     .. Parameters ..
      Integer, Parameter :: nin = 5, nout = 6
!     .. Local Scalars ..
      Integer :: i, ifail, info, lda, ldaf, ldb, ldx, lwork, n, nrhs
      Character (1) :: uplo
!     .. Local Arrays ..
      Real (Kind=dp), Allocatable :: a(:, :), af(:, :), b(:, :), berr(:), &
        ferr(:), work(:), x(:, :)
      Integer, Allocatable :: ipiv(:), iwork(:)
!     .. Executable Statements ..
      Write (nout, *) 'DSYRFS Example Program Results'
!     Skip heading in data file
      Read (nin, *)
      Read (nin, *) n, nrhs
      lda = n
      ldaf = n
      ldb = n
      ldx = n
      lwork = 64*n
      Allocate (a(lda,n), af(ldaf,n), b(ldb,nrhs), berr(nrhs), ferr(nrhs), &
        work(lwork), x(ldx,n), ipiv(n), iwork(n))

!     Read A and B from data file, and copy A to AF and B to X

      Read (nin, *) uplo
      If (uplo=='U') Then
        Do i = 1, n
          Read (nin, *) a(i, i:n)
          af(i, i:n) = a(i, i:n)
        End Do
      Else If (uplo=='L') Then
        Do i = 1, n
          Read (nin, *) a(i, 1:i)
          af(i, 1:i) = a(i, 1:i)
        End Do
      End If
      Read (nin, *)(b(i,1:nrhs), i=1, n)

      x(1:n, 1:nrhs) = b(1:n, 1:nrhs)

!     Factorize A in the array AF
      Call dsytrf(uplo, n, af, ldaf, ipiv, work, lwork, info)

      Write (nout, *)
      Flush (nout)
      If (info==0) Then

!       Compute solution in the array X
        Call dsytrs(uplo, n, nrhs, af, ldaf, ipiv, x, ldx, info)

!       Improve solution, and compute backward errors and
!       estimated bounds on the forward errors
        Call dsyrfs(uplo, n, nrhs, a, lda, af, ldaf, ipiv, b, ldb, x, ldx, &
          ferr, berr, work, iwork, info)

!       Print solution

!       ifail: behaviour on error exit
!              =0 for hard exit, =1 for quiet-soft, =-1 for noisy-soft
        ifail = 0
        Call nagf_file_print_matrix_real_gen('General', ' ', n, nrhs, x, ldx, &
          'Solution(s)', ifail)

        Write (nout, *)
        Write (nout, *) 'Backward errors (machine-dependent)'
        Write (nout, 100) berr(1:nrhs)
        Write (nout, *) 'Estimated forward error bounds (machine-dependent)'
        Write (nout, 100) ferr(1:nrhs)
      Else
        Write (nout, *) 'The factor D is singular'
      End If

100   Format ((3X,1P,7E11.1))
    End Program
