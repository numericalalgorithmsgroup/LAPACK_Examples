    Subroutine nagf_file_print_matrix_complex_gen_comp(matrix, diag, m, n, a, &
      lda, usefrm, form, title, labrow, rlabs, labcol, clabs, ncols, indent, &
      ifail)

!
!     Fortran Library Implementation Wrapper routine for X04DBFN.
!
!     Mark 14 Release. NAG Copyright 1989.
!

!     .. Use Statements ..
      Use lapack_precision, Only: dp
!     .. Implicit None Statement ..
      Implicit None
!     .. Scalar Arguments ..
      Integer :: ifail, indent, lda, m, n, ncols
      Character (1) :: diag, labcol, labrow, matrix, usefrm
      Character (*) :: form, title
!     .. Array Arguments ..
      Complex (Kind=dp) :: a(lda, *)
      Character (*) :: clabs(*), rlabs(*)
!     .. Local Scalars ..
      Integer :: nout
      Character (200) :: errbuf
!     .. External Procedures ..
      External :: x04abfn, x04dbfn
!     .. Executable Statements ..
!
!     Get the advisory channel using X04ABFN
!
      Call x04abfn(0, nout)
!
      Call x04dbfn(matrix, diag, m, n, a, lda, usefrm, form, title, labrow, &
        rlabs, labcol, clabs, ncols, indent, nout, errbuf, ifail)
!
      Return
    End Subroutine
