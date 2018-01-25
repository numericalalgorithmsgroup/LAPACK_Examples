    Subroutine nagf_file_print_matrix_complex_packed_comp(uplo, diag, n, a, &
      usefrm, form, title, labrow, rlabs, labcol, clabs, ncols, indent, ifail)

!     Copyright (c) 2018, Numerical Algorithms Group (NAG Ltd.)
!     For licence see
!       https://github.com/numericalalgorithmsgroup/LAPACK_Examples/blob/master/LICENCE.md

!     .. Use Statements ..
      Use lapack_precision, Only: dp
!     .. Implicit None Statement ..
      Implicit None
!     .. Scalar Arguments ..
      Integer :: ifail, indent, n, ncols
      Character (1) :: diag, labcol, labrow, uplo, usefrm
      Character (*) :: form, title
!     .. Array Arguments ..
      Complex (Kind=dp) :: a(*)
      Character (*) :: clabs(*), rlabs(*)
!     .. Local Scalars ..
      Character (200) :: errbuf
!     .. External Procedures ..
      External :: x04ddfn
!     .. Executable Statements ..
!
      Call x04ddfn(uplo, diag, n, a, usefrm, form, title, labrow, rlabs, &
        labcol, clabs, ncols, indent, errbuf, ifail)
!
      Return
    End Subroutine
