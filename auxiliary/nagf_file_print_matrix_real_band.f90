    Subroutine nagf_file_print_matrix_real_band(m, n, kl, ku, a, lda, title, &
      ifail)

!     Copyright (c) 2018, Numerical Algorithms Group (NAG Ltd.)
!     For licence see
!       https://github.com/numericalalgorithmsgroup/LAPACK_Examples/blob/master/LICENCE.md

!     .. Use Statements ..
      Use lapack_precision, Only: dp
!     .. Implicit None Statement ..
      Implicit None
!     .. Scalar Arguments ..
      Integer :: ifail, kl, ku, lda, m, n
      Character (*) :: title
!     .. Array Arguments ..
      Real (Kind=dp) :: a(lda, *)
!     .. Local Scalars ..
      Character (200) :: errbuf
!     .. External Procedures ..
      External :: x04cefn
!     .. Executable Statements ..
!
      Call x04cefn(m, n, kl, ku, a, lda, title, errbuf, ifail)
!
      Return
    End Subroutine
