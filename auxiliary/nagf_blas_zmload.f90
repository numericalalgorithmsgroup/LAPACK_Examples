    Subroutine nagf_blas_zmload(matrix, m, n, con, diag, a, lda)
!     Mark 13 Release. NAG Copyright 1988.

!     NAGF_BLAS_ZMLOAD forms the m by n matrix A given by

!     a( i, j ) = (  diag  i.eq.j,
!                  (
!                  ( const  i.ne.j.

!     If   MATRIX = 'G' or 'g'   then  A  is regarded  as a general matrix,
!     if   MATRIX = 'U' or 'u'   then  A  is regarded  as upper triangular,
!                              and only  elements  for which  i.le.j  are
!                              referenced,
!     if   MATRIX = 'L' or 'l'   then  A  is regarded  as lower triangular,
!                              and only  elements  for which  i.ge.j  are
!                              referenced.

!     Nag Fortran 77 O( n**2 ) basic linear algebra routine.

!     -- Written on 21-November-1986.
!     Sven Hammarling, Nag Central Office.

!     .. Use Statements ..
      Use lapack_precision, Only: dp
!     .. Implicit None Statement ..
      Implicit None
!     .. Scalar Arguments ..
      Complex (Kind=dp), Intent (In) :: con, diag
      Integer, Intent (In) :: lda, m, n
      Character (1), Intent (In) :: matrix
!     .. Array Arguments ..
      Complex (Kind=dp), Intent (Inout) :: a(lda, *)
!     .. Local Scalars ..
      Integer :: i, j
!     .. Intrinsic Procedures ..
      Intrinsic :: min
!     .. Executable Statements ..
      Continue

      If ((matrix=='G') .Or. (matrix=='g')) Then
        Do j = 1, n
          Do i = 1, m
            a(i, j) = con
          End Do
        End Do
        If (con/=diag) Then
          Do i = 1, min(m, n)
            a(i, i) = diag
          End Do
        End If
      Else If ((matrix=='U') .Or. (matrix=='u')) Then
        Do j = 1, n
          Do i = 1, min(m, j)
            a(i, j) = con
          End Do
        End Do
        If (con/=diag) Then
          Do i = 1, min(m, n)
            a(i, i) = diag
          End Do
        End If
      Else If ((matrix=='L') .Or. (matrix=='l')) Then
        Do j = 1, min(m, n)
          Do i = j, m
            a(i, j) = con
          End Do
        End Do
        If (con/=diag) Then
          Do i = 1, min(m, n)
            a(i, i) = diag
          End Do
        End If
      End If

      Return

!     End of NAGF_BLAS_ZMLOAD.

    End Subroutine
