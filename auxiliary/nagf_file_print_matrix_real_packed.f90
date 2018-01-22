    Subroutine nagf_file_print_matrix_real_packed(uplo, diag, n, a, title, &
      ifail)

!
!     Fortran Library Implementation Wrapper routine for X04CCFN.
!
!     Mark 14 Release. NAG Copyright 1989.
!

!     .. Use Statements ..
      Use lapack_precision, Only: dp
!     .. Implicit None Statement ..
      Implicit None
!     .. Scalar Arguments ..
      Integer :: ifail, n
      Character (1) :: diag, uplo
      Character (*) :: title
!     .. Array Arguments ..
      Real (Kind=dp) :: a(*)
!     .. Local Scalars ..
      Character (200) :: errbuf
!     .. External Procedures ..
      External :: x04ccfn
!     .. Executable Statements ..
!
      Call x04ccfn(uplo, diag, n, a, title, errbuf, ifail)
!
      Return
    End Subroutine
