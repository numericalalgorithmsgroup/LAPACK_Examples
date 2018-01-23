    Program dstegr_example

!     DSTEGR Example Program Text

!     Copyright (c) 2018, Numerical Algorithms Group (NAG Ltd.)
!     For licence see
!       https://github.com/numericalalgorithmsgroup/LAPACK_Examples/blob/master/LICENCE.md

!     .. Use Statements ..
      Use lapack_example_aux, Only: nagf_file_print_matrix_real_gen
      Use lapack_interfaces, Only: dstegr
      Use lapack_precision, Only: dp
!     .. Implicit None Statement ..
      Implicit None
!     .. Parameters ..
      Real (Kind=dp), Parameter :: vl = 0.0E0_dp
      Real (Kind=dp), Parameter :: vu = 0.0E0_dp
      Integer, Parameter :: il = 0, iu = 0, nin = 5, nout = 6
      Character (1), Parameter :: range = 'A'
!     .. Local Scalars ..
      Real (Kind=dp) :: abstol
      Integer :: i, ifail, info, ldz, liwork, lwork, m, n
      Character (1) :: jobz
!     .. Local Arrays ..
      Real (Kind=dp), Allocatable :: d(:), e(:), w(:), work(:), z(:, :)
      Integer, Allocatable :: isuppz(:), iwork(:)
!     .. Executable Statements ..
      Write (nout, *) 'DSTEGR Example Program Results'
      Write (nout, *)
!     Skip heading in data file
      Read (nin, *)
      Read (nin, *) n
      ldz = n
      liwork = 10*n
      lwork = 18*n
      Allocate (d(n), e(n), w(n), work(lwork), z(ldz,n), isuppz(2*n), &
        iwork(liwork))

!     Read the symmetric tridiagonal matrix T from data file, first
!     the diagonal elements, then the off diagonal elements and then
!     JOBV ('N' - eigenvalues only, 'V' - vectors as well)

      Read (nin, *) d(1:n)
      Read (nin, *) e(1:n-1)
      Read (nin, *) jobz

!     Calculate all the eigenvalues of T. Set ABSTOL to zero so that
!     the default value is used.

      abstol = 0.0E0_dp
      Call dstegr(jobz, range, n, d, e, vl, vu, il, iu, abstol, m, w, z, ldz, &
        isuppz, work, lwork, iwork, liwork, info)

      If (info==0) Then

!       Print eigenvalues and eigenvectors

        Write (nout, *) 'Eigenvalues'
        Write (nout, 100) w(1:m)

        Write (nout, *)
        Flush (nout)

!       Standardize the eigenvectors so that first elements are non-negative.
        Do i = 1, m
          If (z(1,i)<0.0_dp) Then
            z(1:n, i) = -z(1:n, i)
          End If
        End Do

!       ifail: behaviour on error exit
!              =0 for hard exit, =1 for quiet-soft, =-1 for noisy-soft
        ifail = 0
        Call nagf_file_print_matrix_real_gen('General', ' ', n, m, z, ldz, &
          'Eigenvectors', ifail)

      Else
        Write (nout, 110) 'Failure to compute an eigenvalue, INFO = ', info
      End If

100   Format ((3X,8F8.4))
110   Format (1X, A, I10)
    End Program
