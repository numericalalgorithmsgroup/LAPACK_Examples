    Subroutine nagf_file_print_matrix_real_gen(matrix, diag, m, n, a, lda, &
      title, ifail)

!
!     Fortran Library Implementation Wrapper routine for X04CAFN.
!
!     Mark 14 Release. NAG Copyright 1989.
!

!     .. Use Statements ..
      Use lapack_precision, Only: dp
!     .. Implicit None Statement ..
      Implicit None
!     .. Scalar Arguments ..
      Integer :: ifail, lda, m, n
      Character (1) :: diag, matrix
      Character (*) :: title
!     .. Array Arguments ..
      Real (Kind=dp) :: a(lda, *)
!     .. Local Scalars ..
      Integer :: nout
      Character (200) :: errbuf
!     .. External Procedures ..
      External :: x04abfn, x04cafn
!     .. Executable Statements ..
!
!     Get the advisory channel using X04ABFN
!
      Call x04abfn(0, nout)
!
      Call x04cafn(matrix, diag, m, n, a, lda, title, nout, errbuf, ifail)
!
      Return
    End Subroutine
