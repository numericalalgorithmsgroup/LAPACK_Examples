    Program zhbgv_example

!     ZHBGV Example Program Text

!     Copyright (c) 2018, Numerical Algorithms Group (NAG Ltd.)
!     For licence see
!       https://github.com/numericalalgorithmsgroup/LAPACK_Examples/blob/master/LICENCE.md

!     .. Use Statements ..
      Use lapack_interfaces, Only: zhbgv
      Use lapack_precision, Only: dp
!     .. Implicit None Statement ..
      Implicit None
!     .. Parameters ..
      Integer, Parameter :: nin = 5, nout = 6
      Character (1), Parameter :: uplo = 'U'
!     .. Local Scalars ..
      Integer :: i, info, j, ka, kb, ldab, ldbb, n
!     .. Local Arrays ..
      Complex (Kind=dp), Allocatable :: ab(:, :), bb(:, :), work(:)
      Complex (Kind=dp) :: dummy(1, 1)
      Real (Kind=dp), Allocatable :: rwork(:), w(:)
!     .. Intrinsic Procedures ..
      Intrinsic :: max, min
!     .. Executable Statements ..
      Write (nout, *) 'ZHBGV Example Program Results'
      Write (nout, *)
!     Skip heading in data file
      Read (nin, *)
      Read (nin, *) n, ka, kb
      ldab = ka + 1
      ldbb = kb + 1
      Allocate (ab(ldab,n), bb(ldbb,n), work(n), rwork(3*n), w(n))

!     Read the upper or lower triangular parts of the matrices A and
!     B from data file

      If (uplo=='U') Then
        Read (nin, *)((ab(ka+1+i-j,j),j=i,min(n,i+ka)), i=1, n)
        Read (nin, *)((bb(kb+1+i-j,j),j=i,min(n,i+kb)), i=1, n)
      Else If (uplo=='L') Then
        Read (nin, *)((ab(1+i-j,j),j=max(1,i-ka),i), i=1, n)
        Read (nin, *)((bb(1+i-j,j),j=max(1,i-kb),i), i=1, n)
      End If

!     Solve the generalized Hermitian band eigenvalue problem
!     A*x = lambda*B*x

      Call zhbgv('No vectors', uplo, n, ka, kb, ab, ldab, bb, ldbb, w, dummy, &
        1, work, rwork, info)

      If (info==0) Then

!       Print solution

        Write (nout, *) 'Eigenvalues'
        Write (nout, 100) w(1:n)
      Else If (info>n .And. info<=2*n) Then
        i = info - n
        Write (nout, 110) 'The leading minor of order ', i, &
          ' of B is not positive definite'
      Else
        Write (nout, 120) 'Failure in ZHBGV. INFO =', info
      End If

100   Format (3X, (6F11.4))
110   Format (1X, A, I4, A)
120   Format (1X, A, I4)
    End Program
