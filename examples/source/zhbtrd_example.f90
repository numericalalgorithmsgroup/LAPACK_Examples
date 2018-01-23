    Program zhbtrd_example

!     ZHBTRD Example Program Text

!     Copyright (c) 2018, Numerical Algorithms Group (NAG Ltd.)
!     For licence see
!       https://github.com/numericalalgorithmsgroup/LAPACK_Examples/blob/master/LICENCE.md

!     .. Use Statements ..
      Use blas_interfaces, Only: dznrm2
      Use lapack_example_aux, Only: nagf_file_print_matrix_complex_gen_comp
      Use lapack_interfaces, Only: zhbtrd, zsteqr
      Use lapack_precision, Only: dp
!     .. Implicit None Statement ..
      Implicit None
!     .. Parameters ..
      Integer, Parameter :: nin = 5, nout = 6
!     .. Local Scalars ..
      Complex (Kind=dp) :: scal
      Integer :: i, ifail, info, j, k, kd, ldab, ldq, n
      Character (1) :: uplo
!     .. Local Arrays ..
      Complex (Kind=dp), Allocatable :: ab(:, :), q(:, :), work(:)
      Real (Kind=dp), Allocatable :: d(:), e(:), rwork(:)
      Character (1) :: clabs(1), rlabs(1)
!     .. Intrinsic Procedures ..
      Intrinsic :: abs, conjg, max, maxloc, min
!     .. Executable Statements ..
      Write (nout, *) 'ZHBTRD Example Program Results'
!     Skip heading in data file
      Read (nin, *)
      Read (nin, *) n, kd
      ldab = kd + 1
      ldq = n
      Allocate (ab(ldab,n), q(ldq,n), work(n), d(n), e(n-1), rwork(2*n-2))

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

!     Reduce A to tridiagonal form T = (Q**H)*A*Q (and form Q)
      Call zhbtrd('V', uplo, n, kd, ab, ldab, d, e, q, ldq, work, info)

!     Calculate all the eigenvalues and eigenvectors of A
      Call zsteqr('V', n, d, e, q, ldq, rwork, info)

      Write (nout, *)
      If (info>0) Then
        Write (nout, *) 'Failure to converge.'
      Else

!       Print eigenvalues and eigenvectors

        Write (nout, *) 'Eigenvalues'
        Write (nout, 100) d(1:n)
        Write (nout, *)
        Flush (nout)

!       Normalize the eigenvectors, largest element real
        Do i = 1, n
          rwork(1:n) = abs(q(1:n,i))
          k = maxloc(rwork(1:n), 1)
          scal = conjg(q(k,i))/abs(q(k,i))/dznrm2(n, q(1,i), 1)
          q(1:n, i) = q(1:n, i)*scal
        End Do

!       ifail: behaviour on error exit
!              =0 for hard exit, =1 for quiet-soft, =-1 for noisy-soft
        ifail = 0
        Call nagf_file_print_matrix_complex_gen_comp('General', ' ', n, n, q, &
          ldq, 'Bracketed', 'F7.4', 'Eigenvectors', 'Integer', rlabs, &
          'Integer', clabs, 80, 0, ifail)

      End If

100   Format (8X, 4(F7.4,11X,:))
    End Program
