    Subroutine x04aafn(iflag, nerr)

!     Copyright (c) 2018, Numerical Algorithms Group (NAG Ltd.)
!     For licence see
!       https://github.com/numericalalgorithmsgroup/LAPACK_Examples/blob/master/LICENCE.md

!     If IFLAG = 0, sets NERR to current error message unit number
!     (stored in NERR1).
!     If IFLAG = 1, changes current error message unit number to
!     value specified by NERR.
!
!     .. Implicit None Statement ..
      Implicit None
!     .. Scalar Arguments ..
      Integer :: iflag, nerr
!     .. Local Scalars ..
      Integer, Save :: nerr1
!     .. Data Statements ..
      Data nerr1/6/
!     .. Executable Statements ..
      Continue
!
!     No argument checking.
!
      If (iflag==0) nerr = nerr1
      If (iflag==1) nerr1 = nerr
      Return
    End Subroutine
