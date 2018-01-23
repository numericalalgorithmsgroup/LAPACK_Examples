    Program zupgtr_example

!     ZUPGTR Example Program Text

!     Copyright (c) 2018, Numerical Algorithms Group (NAG Ltd.)
!     For licence see
!       https://github.com/numericalalgorithmsgroup/LAPACK_Examples/blob/master/LICENCE.md

!     .. Use Statements ..
      Use blas_interfaces, Only: dznrm2
      Use lapack_example_aux, Only: nagf_file_print_matrix_complex_gen_comp
      Use lapack_interfaces, Only: zhptrd, zsteqr, zupgtr
      Use lapack_precision, Only: dp
!     .. Implicit None Statement ..
      Implicit None
!     .. Parameters ..
      Integer, Parameter :: nin = 5, nout = 6
!     .. Local Scalars ..
      Complex (Kind=dp) :: scal
      Integer :: i, ifail, info, j, k, ldq, n
      Character (1) :: uplo
!     .. Local Arrays ..
      Complex (Kind=dp), Allocatable :: ap(:), q(:, :), tau(:), work(:)
      Real (Kind=dp), Allocatable :: d(:), e(:), rwork(:)
      Character (1) :: clabs(1), rlabs(1)
!     .. Intrinsic Procedures ..
      Intrinsic :: abs, conjg, maxloc
!     .. Executable Statements ..
      Write (nout, *) 'ZUPGTR Example Program Results'
!     Skip heading in data file
      Read (nin, *)
      Read (nin, *) n
      ldq = n
      Allocate (ap(n*(n+1)/2), q(ldq,n), tau(n), work(n-1), d(n), e(n), rwork( &
        2*n-2))

!     Read A from data file

      Read (nin, *) uplo
      If (uplo=='U') Then
        Read (nin, *)((ap(i+j*(j-1)/2),j=i,n), i=1, n)
      Else If (uplo=='L') Then
        Read (nin, *)((ap(i+(2*n-j)*(j-1)/2),j=1,i), i=1, n)
      End If

!     Reduce A to tridiagonal form T = (Q**H)*A*Q
      Call zhptrd(uplo, n, ap, d, e, tau, info)

!     Form Q explicitly, storing the result in Q
      Call zupgtr(uplo, n, ap, tau, q, ldq, work, info)

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
