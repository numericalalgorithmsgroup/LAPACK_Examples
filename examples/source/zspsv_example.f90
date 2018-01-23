    Program zspsv_example

!     ZSPSV Example Program Text

!     Copyright (c) 2018, Numerical Algorithms Group (NAG Ltd.)
!     For licence see
!       https://github.com/numericalalgorithmsgroup/LAPACK_Examples/blob/master/LICENCE.md

!     .. Use Statements ..
      Use lapack_example_aux, Only: nagf_file_print_matrix_complex_packed_comp
      Use lapack_interfaces, Only: zspsv
      Use lapack_precision, Only: dp
!     .. Implicit None Statement ..
      Implicit None
!     .. Parameters ..
      Integer, Parameter :: nin = 5, nout = 6
      Character (1), Parameter :: uplo = 'U'
!     .. Local Scalars ..
      Integer :: i, ifail, info, j, n
!     .. Local Arrays ..
      Complex (Kind=dp), Allocatable :: ap(:), b(:)
      Integer, Allocatable :: ipiv(:)
      Character (1) :: clabs(1), rlabs(1)
!     .. Executable Statements ..
      Write (nout, *) 'ZSPSV Example Program Results'
      Write (nout, *)
!     Skip heading in data file
      Read (nin, *)
      Read (nin, *) n

      Allocate (ap((n*(n+1))/2), b(n), ipiv(n))

!     Read the upper or lower triangular part of the matrix A from
!     data file

      If (uplo=='U') Then
        Read (nin, *)((ap(i+(j*(j-1))/2),j=i,n), i=1, n)
      Else If (uplo=='L') Then
        Read (nin, *)((ap(i+((2*n-j)*(j-1))/2),j=1,i), i=1, n)
      End If

!     Read b from data file

      Read (nin, *) b(1:n)

!     Solve the equations Ax = b for x
      Call zspsv(uplo, n, 1, ap, ipiv, b, n, info)

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
        Call nagf_file_print_matrix_complex_packed_comp(uplo, &
          'Non-unit diagonal', n, ap, 'Bracketed', 'F7.4', &
          'Details of factorization', 'Integer', rlabs, 'Integer', clabs, 80, &
          0, ifail)

!       Print pivot indices

        Write (nout, *)
        Write (nout, *) 'Pivot indices'
        Write (nout, 110) ipiv(1:n)

      Else
        Write (nout, 120) 'The diagonal block ', info, ' of D is zero'
      End If

100   Format ((3X,4(' (',F7.4,',',F7.4,')',:)))
110   Format (1X, 7I11)
120   Format (1X, A, I3, A)
    End Program
