    Program dgetri_example

!     DGETRI Example Program Text

!     Copyright 2017, Numerical Algorithms Group Ltd. http://www.nag.com

!     .. Use Statements ..
      Use lapack_example_aux, Only: nagf_file_print_matrix_real_gen
      Use lapack_interfaces, Only: dgetrf, dgetri
      Use lapack_precision, Only: dp
!     .. Implicit None Statement ..
      Implicit None
!     .. Parameters ..
      Integer, Parameter :: nin = 5, nout = 6
!     .. Local Scalars ..
      Integer :: i, ifail, info, lda, lwork, n
!     .. Local Arrays ..
      Real (Kind=dp), Allocatable :: a(:, :), work(:)
      Integer, Allocatable :: ipiv(:)
!     .. Executable Statements ..
      Write (nout, *) 'DGETRI Example Program Results'
!     Skip heading in data file
      Read (nin, *)
      Read (nin, *) n
      lda = n
      lwork = 64*n
      Allocate (a(lda,n), work(lwork), ipiv(n))

!     Read A from data file

      Read (nin, *)(a(i,1:n), i=1, n)

!     Factorize A

      Call dgetrf(n, n, a, lda, ipiv, info)

      Write (nout, *)
      Flush (nout)
      If (info==0) Then

!       Compute inverse of A

        Call dgetri(n, a, lda, ipiv, work, lwork, info)

!       Print inverse

!       ifail: behaviour on error exit
!              =0 for hard exit, =1 for quiet-soft, =-1 for noisy-soft
        ifail = 0
        Call nagf_file_print_matrix_real_gen('General', ' ', n, n, a, lda, &
          'Inverse', ifail)

      Else
        Write (nout, *) 'The factor U is singular'
      End If

    End Program
