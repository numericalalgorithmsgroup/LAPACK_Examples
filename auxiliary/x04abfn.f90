    Subroutine x04abfn(iflag, nadv)

!     Copyright (c) 2018, Numerical Algorithms Group (NAG Ltd.)
!     For licence see
!       https://github.com/numericalalgorithmsgroup/LAPACK_Examples/blob/master/LICENCE.md

!      If IFLAG = 0, sets NADV to current advisory message unit number
!     (stored in NADV1).
!     If IFLAG = 1, changes current advisory message unit number to
!     value specified by NADV.
!
!     .. Implicit None Statement ..
      Implicit None
!     .. Scalar Arguments ..
      Integer :: iflag, nadv
!     .. Local Scalars ..
      Integer, Save :: nadv1
!     .. Data Statements ..
      Data nadv1/6/
!     .. Executable Statements ..
      Continue
!
!     No argument checking.
!
      If (iflag==0) nadv = nadv1
      If (iflag==1) nadv1 = nadv
      Return
    End Subroutine
