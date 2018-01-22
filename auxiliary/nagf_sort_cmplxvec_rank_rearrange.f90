    Subroutine nagf_sort_cmplxvec_rank_rearrange(cv, m1, m2, irank, ifail)
!     Mark 19 Release. NAG Copyright 1999.
!
!     NAGF_SORT_CMPLXVEC_RANK_REARRANGE re-arranges a vector of complex numbers
!     into the order specified by a vector of ranks.
!
!     NAGF_SORT_CMPLXVEC_RANK_REARRANGE is designed to be used typically in
!     conjunction with the M01D- ranking routines. After one of the M01D-
!     routines has been called to determine a vector of ranks,
!     NAGF_SORT_CMPLXVEC_RANK_REARRANGE can be called to re-arrange a vector
!     of complex numbers into the rank order. If the vector of ranks has
!     been generated in some other way, then M01ZBF should be
!     called to check its validity before NAGF_SORT_CMPLXVEC_RANK_REARRANGE
!     is called.
!
!     Written by N.M.Maclaren, University of Cambridge.
!     Revised by NAG Central Office.
!

!     .. Use Statements ..
      Use lapack_precision, Only: dp
!     .. Implicit None Statement ..
      Implicit None
!     .. Parameters ..
      Character (6), Parameter :: srname = 'M01EDF'
!     .. Scalar Arguments ..
      Integer :: ifail, m1, m2
!     .. Array Arguments ..
      Complex (Kind=dp) :: cv(m2)
      Integer :: irank(m2)
!     .. Local Scalars ..
      Complex (Kind=dp) :: a, b
      Integer :: i, ierr, j, k
!     .. Local Arrays ..
      Character (80) :: p01rec(2)
!     .. External Procedures ..
      Integer, External :: p01abf
!     .. Intrinsic Procedures ..
      Intrinsic :: abs
!     .. Executable Statements ..
!
!     Check the parameters and modify irank.
!
      If (m2<1 .Or. m1<1 .Or. m1>m2) Then
        ierr = 1
        Write (p01rec, Fmt=150) m1, m2
      Else
        ierr = 0
        Do i = m1, m2
          j = irank(i)
          If ((j<m1) .Or. (j>m2)) Go To 120
          If (i/=j) irank(i) = -j
        End Do
!
!       Move each non-trivial cycle round.
!
        Do i = m1, m2
          k = -irank(i)
          If (k>=0) Then
            j = i
            a = cv(i)
100         irank(j) = k
            b = cv(k)
            cv(k) = a
            j = k
            a = b
            k = -irank(j)
            If (k>0) Go To 100
            If (j/=i) Go To 130
          End If
        End Do
      End If
!
!     Return
!
110   If (ierr/=0) Then
        ifail = p01abf(ifail, ierr, srname, 2, p01rec)
      Else
        ifail = 0
      End If
      Return
120   ierr = 2
      Write (p01rec(2), Fmt=170) i, j
      Go To 140
130   ierr = 3
      Write (p01rec(2), Fmt=180) j
140   Write (p01rec(1), Fmt=160)
!
!     Restore IRANK
!
      Do j = m1, m2
        irank(j) = abs(irank(j))
      End Do
      Go To 110
!
150   Format (' ** On entry, one or more of the following parameter va', &
        'lues is illegal', /, '    M1 =', I16, '  M2 =', I16)
160   Format (' ** IRANK(M1:M2) does not contain a permutation of the ', &
        'integers M1 to M2')
170   Format ('    IRANK(', I6, ') contains an out-of-range value', I16)
180   Format ('    IRANK contains a repeated value', I16)
    End Subroutine
