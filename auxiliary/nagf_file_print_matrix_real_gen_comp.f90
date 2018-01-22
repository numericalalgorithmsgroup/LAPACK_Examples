    Subroutine nagf_file_print_matrix_real_gen_comp(matrix, diag, m, n, a, &
      lda, form, title, labrow, rlabs, labcol, clabs, ncols, indent, ifail)

!
!     Fortran Library Implementation Wrapper routine for X04CBFN.
!
!     Mark 14 Release. NAG Copyright 1989.
!

!     .. Use Statements ..
      Use lapack_precision, Only: dp
!     .. Implicit None Statement ..
      Implicit None
!     .. Scalar Arguments ..
      Integer :: ifail, indent, lda, m, n, ncols
      Character (1) :: diag, labcol, labrow, matrix
      Character (*) :: form, title
!     .. Array Arguments ..
      Real (Kind=dp) :: a(lda, *)
      Character (*) :: clabs(*), rlabs(*)
!     .. Local Scalars ..
      Integer :: nout
      Character (200) :: errbuf
!     .. External Procedures ..
      External :: x04abfn, x04cbfn
!     .. Executable Statements ..
!
!     Get the advisory channel using X04ABFN
!
      Call x04abfn(0, nout)
!
      Call x04cbfn(matrix, diag, m, n, a, lda, form, title, labrow, rlabs, &
        labcol, clabs, ncols, indent, nout, errbuf, ifail)
!
      Return
    End Subroutine
