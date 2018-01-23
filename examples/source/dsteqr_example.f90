    Program dsteqr_example

!     DSTEQR Example Program Text

!     Copyright (c) 2018, Numerical Algorithms Group (NAG Ltd.)
!     For licence see
!       https://github.com/numericalalgorithmsgroup/LAPACK_Examples/blob/master/LICENCE.md

!     .. Use Statements ..
      Use lapack_example_aux, Only: nagf_file_print_matrix_real_gen
      Use lapack_interfaces, Only: dsteqr
      Use lapack_precision, Only: dp
!     .. Implicit None Statement ..
      Implicit None
!     .. Parameters ..
      Integer, Parameter :: nin = 5, nout = 6
!     .. Local Scalars ..
      Integer :: i, ifail, info, ldz, n
!     .. Local Arrays ..
      Real (Kind=dp), Allocatable :: d(:), e(:), work(:), z(:, :)
!     .. Executable Statements ..
      Write (nout, *) 'DSTEQR Example Program Results'
!     Skip heading in data file
      Read (nin, *)
      Read (nin, *) n
      ldz = n
      Allocate (d(n), e(n-1), work(2*n-2), z(ldz,n))

!     Read T from data file

      Read (nin, *) d(1:n)
      Read (nin, *) e(1:n-1)

!     Calculate all the eigenvalues and eigenvectors of T
      Call dsteqr('I', n, d, e, z, ldz, work, info)

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
          If (z(1,i)<0.0_dp) Then
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
