    Program dopgtr_example

!     DOPGTR Example Program Text

!     Copyright 2017, Numerical Algorithms Group Ltd. http://www.nag.com

!     .. Use Statements ..
      Use lapack_example_aux, Only: nagf_blas_damax_val, &
        nagf_file_print_matrix_real_gen
      Use lapack_interfaces, Only: dopgtr, dsptrd, dsteqr
      Use lapack_precision, Only: dp
!     .. Implicit None Statement ..
      Implicit None
!     .. Parameters ..
      Real (Kind=dp), Parameter :: zero = 0.0E0_dp
      Integer, Parameter :: nin = 5, nout = 6
!     .. Local Scalars ..
      Real (Kind=dp) :: r
      Integer :: i, ifail, info, j, k, ldq, n
      Character (1) :: uplo
!     .. Local Arrays ..
      Real (Kind=dp), Allocatable :: ap(:), d(:), e(:), q(:, :), tau(:), &
        work(:)
!     .. Executable Statements ..
      Write (nout, *) 'DOPGTR Example Program Results'
!     Skip heading in data file
      Read (nin, *)
      Read (nin, *) n
      ldq = n
      Allocate (ap(n*(n+1)/2), d(n), e(n), q(ldq,n), tau(n), work(2*n-2))

!     Read A from data file

      Read (nin, *) uplo
      If (uplo=='U') Then
        Read (nin, *)((ap(i+j*(j-1)/2),j=i,n), i=1, n)
      Else If (uplo=='L') Then
        Read (nin, *)((ap(i+(2*n-j)*(j-1)/2),j=1,i), i=1, n)
      End If

!     Reduce A to tridiagonal form T = (Q**T)*A*Q
      Call dsptrd(uplo, n, ap, d, e, tau, info)

!     Form Q explicitly, storing the result in Q
      Call dopgtr(uplo, n, ap, tau, q, ldq, work, info)

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

!       Normalize the eigenvectors: largest element positive
        Do i = 1, n
          Call nagf_blas_damax_val(n, q(1,i), 1, k, r)
          If (q(k,i)<zero) Then
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
