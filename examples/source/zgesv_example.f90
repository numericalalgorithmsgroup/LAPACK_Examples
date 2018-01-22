    Program zgesv_example

!     ZGESV Example Program Text

!     Copyright 2017, Numerical Algorithms Group Ltd. http://www.nag.com

!     .. Use Statements ..
      Use lapack_example_aux, Only: nagf_file_print_matrix_complex_gen_comp
      Use lapack_interfaces, Only: zgesv
      Use lapack_precision, Only: dp
!     .. Implicit None Statement ..
      Implicit None
!     .. Parameters ..
      Integer, Parameter :: nin = 5, nout = 6
!     .. Local Scalars ..
      Integer :: i, ifail, info, lda, ldb, n
!     .. Local Arrays ..
      Complex (Kind=dp), Allocatable :: a(:, :), b(:)
      Integer, Allocatable :: ipiv(:)
      Character (1) :: clabs(1), rlabs(1)
!     .. Executable Statements ..
      Write (nout, *) 'ZGESV Example Program Results'
      Write (nout, *)
!     Skip heading in data file
      Read (nin, *)
      Read (nin, *) n
      lda = n
      ldb = n
      Allocate (a(lda,n), b(ldb), ipiv(n))

!     Read A and B from data file

      Read (nin, *)(a(i,1:n), i=1, n)
      Read (nin, *) b(1:n)

!     Solve the equations Ax = b for x

      Call zgesv(n, 1, a, lda, ipiv, b, ldb, info)

      If (info==0) Then

!       Print solution

        Write (nout, *) 'Solution'
        Write (nout, 100) b(1:n)

!       Print details of factorization

        Write (nout, *)
        Flush (nout)

!       ifail: behaviour on error exit
!              =0 for hard exit, =1 for quiet-soft, =-1 for noisy-soft
        ifail = 0
        Call nagf_file_print_matrix_complex_gen_comp('General', ' ', n, n, a, &
          lda, 'Bracketed', 'F7.4', 'Details of factorization', 'Integer', &
          rlabs, 'Integer', clabs, 80, 0, ifail)

!       Print pivot indices

        Write (nout, *)
        Write (nout, *) 'Pivot indices'
        Write (nout, 110) ipiv(1:n)
      Else
        Write (nout, 120) 'The (', info, ',', info, ')', &
          ' element of the factor U is zero'
      End If

100   Format ((3X,4(' (',F7.4,',',F7.4,')',:)))
110   Format (1X, 7I11)
120   Format (1X, A, I3, A, I3, A, A)
    End Program
