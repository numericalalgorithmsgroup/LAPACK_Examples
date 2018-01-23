    Program dsbtrd_example

!     DSBTRD Example Program Text

!     Copyright (c) 2018, Numerical Algorithms Group (NAG Ltd.)
!     For licence see
!       https://github.com/numericalalgorithmsgroup/LAPACK_Examples/blob/master/LICENCE.md

!     .. Use Statements ..
      Use lapack_example_aux, Only: nagf_file_print_matrix_real_gen
      Use lapack_interfaces, Only: dsbtrd, dsteqr
      Use lapack_precision, Only: dp
!     .. Implicit None Statement ..
      Implicit None
!     .. Parameters ..
      Integer, Parameter :: nin = 5, nout = 6
!     .. Local Scalars ..
      Integer :: i, ifail, info, j, kd, ldab, ldq, n
      Character (1) :: uplo
!     .. Local Arrays ..
      Real (Kind=dp), Allocatable :: ab(:, :), d(:), e(:), q(:, :), work(:)
!     .. Intrinsic Procedures ..
      Intrinsic :: max, min
!     .. Executable Statements ..
      Write (nout, *) 'DSBTRD Example Program Results'
!     Skip heading in data file
      Read (nin, *)
      Read (nin, *) n, kd
      ldab = kd + 1
      ldq = n
      Allocate (ab(ldab,n), d(n), e(n-1), q(ldq,n), work(2*n-2))

!     Read A from data file

      Read (nin, *) uplo
      If (uplo=='U') Then
        Do i = 1, n
          Read (nin, *)(ab(kd+1+i-j,j), j=i, min(n,i+kd))
        End Do
      Else If (uplo=='L') Then
        Do i = 1, n
          Read (nin, *)(ab(1+i-j,j), j=max(1,i-kd), i)
        End Do
      End If

!     Reduce A to tridiagonal form T = (Q**T)*A*Q (and form Q)
      Call dsbtrd('V', uplo, n, kd, ab, ldab, d, e, q, ldq, work, info)

!     Calculate all the eigenvalues and eigenvectors of A
      Call dsteqr('V', n, d, e, q, ldq, work, info)

      Write (nout, *)
      If (info>0) Then
        Write (nout, *) 'Failure to converge.'
      Else

!       Print eigenvalues and eigenvectors

        Write (nout, *) 'Eigenvalues'
        Write (nout, 100) d(1:n)
        Write (nout, *)
        Flush (nout)

!       Standardize the eigenvectors so that first elements are non-negative.
        Do i = 1, n
          If (q(1,i)<0.0_dp) Then
            q(1:n, i) = -q(1:n, i)
          End If
        End Do

!       ifail: behaviour on error exit
!              =0 for hard exit, =1 for quiet-soft, =-1 for noisy-soft
        ifail = 0
        Call nagf_file_print_matrix_real_gen('General', ' ', n, n, q, ldq, &
          'Eigenvectors', ifail)

      End If

100   Format (3X, (8F8.4))
    End Program
