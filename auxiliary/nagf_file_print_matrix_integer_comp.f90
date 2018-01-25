    Subroutine nagf_file_print_matrix_integer_comp(matrix, diag, m, n, a, lda, &
      form, title, labrow, rlabs, labcol, clabs, ncols, indent, ifail)

!     Copyright (c) 2018, Numerical Algorithms Group (NAG Ltd.)
!     For licence see
!       https://github.com/numericalalgorithmsgroup/LAPACK_Examples/blob/master/LICENCE.md

!     .. Implicit None Statement ..
      Implicit None
!     .. Scalar Arguments ..
      Integer :: ifail, indent, lda, m, n, ncols
      Character (1) :: diag, labcol, labrow, matrix
      Character (*) :: form, title
!     .. Array Arguments ..
      Integer :: a(lda, *)
      Character (*) :: clabs(*), rlabs(*)
!     .. Local Scalars ..
      Character (200) :: errbuf
!     .. External Procedures ..
      External :: x04ebfn
!     .. Executable Statements ..
      Continue
      Call x04ebfn(matrix, diag, m, n, a, lda, form, title, labrow, rlabs, &
        labcol, clabs, ncols, indent, errbuf, ifail)
!
      Return
    End Subroutine
