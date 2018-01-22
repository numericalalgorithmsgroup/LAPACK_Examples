    Subroutine nagf_file_print_matrix_complex_band(m, n, kl, ku, a, lda, &
      title, ifail)
!
!     Fortran Library Implementation Wrapper routine for X04DEFN.
!
!     .. Use Statements ..
      Use lapack_precision, Only: dp
!     .. Implicit None Statement ..
      Implicit None
!     .. Scalar Arguments ..
      Integer :: ifail, kl, ku, lda, m, n
      Character (*) :: title
!     .. Array Arguments ..
      Complex (Kind=dp) :: a(lda, *)
!     .. Local Scalars ..
      Character (200) :: errbuf
!     .. External Procedures ..
      External :: x04defn
!     .. Executable Statements ..
      Continue
      Call x04defn(m, n, kl, ku, a, lda, title, errbuf, ifail)
!
      Return
    End Subroutine
