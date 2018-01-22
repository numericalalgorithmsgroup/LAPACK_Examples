    Program dstevd_example

!     DSTEVD Example Program Text

!     Copyright 2017, Numerical Algorithms Group Ltd. http://www.nag.com

!     .. Use Statements ..
      Use blas_interfaces, Only: dnrm2
      Use lapack_example_aux, Only: nagf_file_print_matrix_real_gen
      Use lapack_interfaces, Only: dstevd
      Use lapack_precision, Only: dp
!     .. Implicit None Statement ..
      Implicit None
!     .. Parameters ..
      Integer, Parameter :: nin = 5, nout = 6
!     .. Local Scalars ..
      Real (Kind=dp) :: norm
      Integer :: i, ifail, info, ldz, liwork, lwork, n
      Character (1) :: job
!     .. Local Arrays ..
      Real (Kind=dp), Allocatable :: d(:), e(:), work(:), z(:, :)
      Integer, Allocatable :: iwork(:)
!     .. Executable Statements ..
      Write (nout, *) 'DSTEVD Example Program Results'
!     Skip heading in data file
      Read (nin, *)
      Read (nin, *) n
      ldz = n
      liwork = 5*n + 3
      lwork = n*n + 4*n + 1
      Allocate (d(n), e(n-1), work(lwork), z(ldz,n), iwork(liwork))

!     Read T from data file

      Read (nin, *) d(1:n)
      Read (nin, *) e(1:n-1)

      Read (nin, *) job

!     Calculate all the eigenvalues and eigenvectors of T
      Call dstevd(job, n, d, e, z, ldz, work, lwork, iwork, liwork, info)

      Write (nout, *)
      If (info>0) Then
        Write (nout, *) 'Failure to converge.'
      Else

!       Print eigenvalues and eigenvectors

        Write (nout, *) 'Eigenvalues'
        Write (nout, 100) d(1:n)
        Write (nout, *)
        Flush (nout)

!       Normalize the eigenvectors
        Do i = 1, n
          norm = dnrm2(n, z(1,i), 1)
          If (z(1,i)<0.0_dp) Then
            norm = -norm
          End If
          z(1:n, i) = z(1:n, i)/norm
        End Do

!       ifail: behaviour on error exit
!              =0 for hard exit, =1 for quiet-soft, =-1 for noisy-soft
        ifail = 0
        Call nagf_file_print_matrix_real_gen('General', ' ', n, n, z, ldz, &
          'Eigenvectors', ifail)

      End If

100   Format (3X, (8F8.4))
    End Program
