    Program dpteqr_example

!     DPTEQR Example Program Text

!     Copyright 2017, Numerical Algorithms Group Ltd. http://www.nag.com

!     .. Use Statements ..
      Use lapack_example_aux, Only: nagf_blas_damax_val, &
        nagf_file_print_matrix_real_gen
      Use lapack_interfaces, Only: dpteqr
      Use lapack_precision, Only: dp
!     .. Implicit None Statement ..
      Implicit None
!     .. Parameters ..
      Real (Kind=dp), Parameter :: zero = 0.0_dp
      Integer, Parameter :: nin = 5, nout = 6
!     .. Local Scalars ..
      Real (Kind=dp) :: r
      Integer :: i, ifail, info, k, ldz, n
!     .. Local Arrays ..
      Real (Kind=dp), Allocatable :: d(:), e(:), work(:), z(:, :)
!     .. Executable Statements ..
      Write (nout, *) 'DPTEQR Example Program Results'
!     Skip heading in data file
      Read (nin, *)
      Read (nin, *) n
      ldz = n
      Allocate (d(n), e(n-1), work(4*n), z(ldz,n))

!     Read T from data file

      Read (nin, *) d(1:n)
      Read (nin, *) e(1:n-1)

!     Calculate all the eigenvalues and eigenvectors of T
      Call dpteqr('I', n, d, e, z, ldz, work, info)

      Write (nout, *)
      If (info>0 .And. info<=n) Then
        Write (nout, *) 'T is not positive definite.'
      Else If (info>n) Then
        Write (nout, *) 'Failure to converge.'
      Else

!       Print eigenvalues and eigenvectors

        Write (nout, *) 'Eigenvalues'
        Write (nout, 100) d(1:n)
        Write (nout, *)
        Flush (nout)

!       Normalize the eigenvectors, largest positive
        Do i = 1, n
          Call nagf_blas_damax_val(n, z(1,i), 1, k, r)
          If (z(k,i)<zero) Then
            z(1:n, i) = -z(1:n, i)
          End If
        End Do

!       ifail: behaviour on error exit
!              =0 for hard exit, =1 for quiet-soft, =-1 for noisy-soft
        ifail = 0
        Call nagf_file_print_matrix_real_gen('General', ' ', n, n, z, ldz, &
          'Eigenvectors', ifail)

      End If

100   Format (3X, (8F8.4))
    End Program
