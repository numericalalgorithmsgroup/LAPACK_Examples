    Program dgelsd_example

!     DGELSD Example Program Text

!     Copyright (c) 2018, Numerical Algorithms Group (NAG Ltd.)
!     For licence see
!       https://github.com/numericalalgorithmsgroup/LAPACK_Examples/blob/master/LICENCE.md

!     .. Use Statements ..
      Use lapack_interfaces, Only: dgelsd
      Use lapack_precision, Only: dp
!     .. Implicit None Statement ..
      Implicit None
!     .. Parameters ..
      Integer, Parameter :: nin = 5, nout = 6
!     .. Local Scalars ..
      Real (Kind=dp) :: rcond
      Integer :: i, info, lda, liwork, lwork, m, n, rank
!     .. Local Arrays ..
      Real (Kind=dp), Allocatable :: a(:, :), b(:), s(:), work(:)
      Real (Kind=dp) :: lw(1)
      Integer, Allocatable :: iwork(:)
      Integer :: liw(1)
!     .. Intrinsic Procedures ..
      Intrinsic :: nint
!     .. Executable Statements ..
      Write (nout, *) 'DGELSD Example Program Results'
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

      rcond = 0.01_dp

!     Call dgelsd in workspace query mode.
      lwork = -1
      Call dgelsd(m, n, 1, a, lda, b, n, s, rcond, rank, lw, lwork, liw, info)
      lwork = nint(lw(1))
      liwork = liw(1)
      Allocate (work(lwork), iwork(liwork))

!     Now Solve the least squares problem min( norm2(b - Ax) ) for the
!     x of minimum norm.
      Call dgelsd(m, n, 1, a, lda, b, n, s, rcond, rank, work, lwork, iwork, &
        info)

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
        Write (nout, 100) s(1:m)
      Else
        Write (nout, *) 'The SVD algorithm failed to converge'
      End If

100   Format (1X, 7F11.4)
110   Format (3X, 1P, E11.2)
120   Format (1X, I6)
    End Program
