    Subroutine nagf_file_print_matrix_complex_band_comp(m, n, kl, ku, a, lda, &
      usefrm, form, title, labrow, rlabs, labcol, clabs, ncols, indent, ifail)

!
!     Fortran Library Implementation Wrapper routine for X04DFFN.
!
!     Mark 14 Release. NAG Copyright 1989.
!

!     .. Use Statements ..
      Use lapack_precision, Only: dp
!     .. Implicit None Statement ..
      Implicit None
!     .. Scalar Arguments ..
      Integer :: ifail, indent, kl, ku, lda, m, n, ncols
      Character (*) :: form, title
      Character (1) :: labcol, labrow, usefrm
!     .. Array Arguments ..
      Complex (Kind=dp) :: a(lda, *)
      Character (*) :: clabs(*), rlabs(*)
!     .. Local Scalars ..
      Character (200) :: errbuf
!     .. External Procedures ..
      External :: x04dffn
!     .. Executable Statements ..
!
      Call x04dffn(m, n, kl, ku, a, lda, usefrm, form, title, labrow, rlabs, &
        labcol, clabs, ncols, indent, errbuf, ifail)
!
      Return
    End Subroutine
