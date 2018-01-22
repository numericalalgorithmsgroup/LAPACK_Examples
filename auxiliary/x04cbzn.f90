    Subroutine x04cbzn(str, start, finish)
!     Mark 14 Release. NAG Copyright 1989.
!     Returns START as first non blank position of STR, and FINISH
!     as the last non-blank.

!     .. Implicit None Statement ..
      Implicit None
!     .. Scalar Arguments ..
      Integer :: finish, start
      Character (*) :: str
!     .. Local Scalars ..
      Integer :: l
!     .. Intrinsic Procedures ..
      Intrinsic :: len
!     .. Executable Statements ..
      Continue
      If (str==' ') Then
        start = 0
        finish = 0
      Else
        l = len(str)
        start = 1
100     If (str(start:start)==' ' .And. start<l) Then
          start = start + 1
          Go To 100
        End If
        finish = l
110     If (str(finish:finish)==' ' .And. finish>1) Then
          finish = finish - 1
          Go To 110
        End If
      End If
      Return
    End Subroutine
