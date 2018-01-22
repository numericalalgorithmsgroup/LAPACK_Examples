    Program zgelsd_example

!     ZGELSD Example Program Text

!     Copyright 2017, Numerical Algorithms Group Ltd. http://www.nag.com

!     .. Use Statements ..
      Use lapack_interfaces, Only: zgelsd
      Use lapack_precision, Only: dp
!     .. Implicit None Statement ..
      Implicit None
!     .. Parameters ..
      Integer, Parameter :: nin = 5, nout = 6
!     .. Local Scalars ..
      Real (Kind=dp) :: rcond
      Integer :: i, info, lda, liwork, lrwork, lwork, m, n, rank
!     .. Local Arrays ..
      Complex (Kind=dp), Allocatable :: a(:, :), b(:), work(:)
      Complex (Kind=dp) :: lw(1)
      Real (Kind=dp) :: lrw(1)
      Real (Kind=dp), Allocatable :: rwork(:), s(:)
      Integer, Allocatable :: iwork(:)
      Integer :: liw(1)
!     .. Intrinsic Procedures ..
      Intrinsic :: nint, real
!     .. Executable Statements ..
      Write (nout, *) 'ZGELSD Example Program Results'
      Write (nout, *)
!     Skip heading in data file
      Read (nin, *)
      Read (nin, *) m, n
      lda = m
      Allocate (a(lda,n), b(n), s(m))

!     Read A and B from data file

      Read (nin, *)(a(i,1:n), i=1, m)
      Read (nin, *) b(1:m)

!     Choose RCOND to reflect the relative accuracy of the input
!     data

      rcond = 0.01E0_dp

!     Call zgelsd in workspace query mode.
      lwork = -1
      Call zgelsd(m, n, 1, a, lda, b, n, s, rcond, rank, lw, lwork, lrw, liw, &
        info)
      lwork = nint(real(lw(1)))
      lrwork = nint(lrw(1))
      liwork = liw(1)
      Allocate (work(lwork), rwork(lrwork), iwork(liwork))

!     Solve the least squares problem min( norm2(b - Ax) ) for the
!     x of minimum norm.

      Call zgelsd(m, n, 1, a, lda, b, n, s, rcond, rank, work, lwork, rwork, &
        iwork, info)

      If (info==0) Then

!       Print solution

        Write (nout, *) 'Least squares solution'
        Write (nout, 100) b(1:n)

!       Print the effective rank of A

        Write (nout, *)
        Write (nout, *) 'Tolerance used to estimate the rank of A'
        Write (nout, 110) rcond
        Write (nout, *) 'Estimated rank of A'
        Write (nout, 120) rank

!       Print singular values of A

        Write (nout, *)
        Write (nout, *) 'Singular values of A'
        Write (nout, 130) s(1:m)
      Else If (info>0) Then
        Write (nout, *) 'The SVD algorithm failed to converge'
      End If

100   Format (4(' (',F7.4,',',F7.4,')',:))
110   Format (3X, 1P, E11.2)
120   Format (1X, I6)
130   Format (1X, 7F11.4)
    End Program
