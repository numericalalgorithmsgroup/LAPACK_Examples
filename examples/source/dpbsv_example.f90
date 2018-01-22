    Program dpbsv_example

!     DPBSV Example Program Text

!     Copyright 2017, Numerical Algorithms Group Ltd. http://www.nag.com

!     .. Use Statements ..
      Use lapack_example_aux, Only: nagf_file_print_matrix_real_band
      Use lapack_interfaces, Only: dpbsv
      Use lapack_precision, Only: dp
!     .. Implicit None Statement ..
      Implicit None
!     .. Parameters ..
      Integer, Parameter :: nin = 5, nout = 6
      Character (1), Parameter :: uplo = 'U'
!     .. Local Scalars ..
      Integer :: i, ifail, info, j, kd, ldab, n
!     .. Local Arrays ..
      Real (Kind=dp), Allocatable :: ab(:, :), b(:)
!     .. Intrinsic Procedures ..
      Intrinsic :: max, min
!     .. Executable Statements ..
      Write (nout, *) 'DPBSV Example Program Results'
      Write (nout, *)
!     Skip heading in data file
      Read (nin, *)
      Read (nin, *) n, kd
      ldab = kd + 1
      Allocate (ab(ldab,n), b(n))

!     Read the upper or lower triangular part of the band matrix A
!     from data file

      If (uplo=='U') Then
        Read (nin, *)((ab(kd+1+i-j,j),j=i,min(n,i+kd)), i=1, n)
      Else If (uplo=='L') Then
        Read (nin, *)((ab(1+i-j,j),j=max(1,i-kd),i), i=1, n)
      End If

!     Read b from data file

      Read (nin, *) b(1:n)

!     Solve the equations Ax = b for x
      Call dpbsv(uplo, n, kd, 1, ab, ldab, b, n, info)

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
        If (uplo=='U') Then
          Call nagf_file_print_matrix_real_band(n, n, 0, kd, ab, ldab, &
            'Cholesky factor U', ifail)
        Else If (uplo=='L') Then
          Call nagf_file_print_matrix_real_band(n, n, kd, 0, ab, ldab, &
            'Cholesky factor L', ifail)
        End If

      Else
        Write (nout, 110) 'The leading minor of order ', info, &
          ' is not positive definite'
      End If

100   Format ((3X,7F11.4))
110   Format (1X, A, I3, A)
    End Program
