    Program dstedc_example

!     DSTEDC Example Program Text

!     Copyright 2017, Numerical Algorithms Group Ltd. http://www.nag.com

!     .. Use Statements ..
      Use lapack_example_aux, Only: nagf_file_print_matrix_real_gen
      Use lapack_interfaces, Only: dsbtrd, dstedc
      Use lapack_precision, Only: dp
!     .. Implicit None Statement ..
      Implicit None
!     .. Parameters ..
      Integer, Parameter :: nin = 5, nout = 6
      Character (1), Parameter :: uplo = 'U'
!     .. Local Scalars ..
      Integer :: i, ifail, info, j, kd, ldab, ldz, lgn, liwork, lwork, n
!     .. Local Arrays ..
      Real (Kind=dp), Allocatable :: ab(:, :), d(:), e(:), work(:), z(:, :)
      Real (Kind=dp) :: rdum(1)
      Integer :: idum(1)
      Integer, Allocatable :: iwork(:)
!     .. Intrinsic Procedures ..
      Intrinsic :: ceiling, log, max, min, nint, real
!     .. Executable Statements ..
      Write (nout, *) 'DSTEDC Example Program Results'
      Write (nout, *)
!     Skip heading in data file
      Read (nin, *)
      Read (nin, *) n, kd
      ldab = kd + 1
      ldz = n
      lgn = ceiling(log(real(n,kind=dp))/log(2.0E0_dp))
      Allocate (ab(ldab,n), d(n), e(n-1), z(ldz,n))

!     Use routine workspace query to get optimal workspace.
      lwork = -1
      liwork = -1
      Call dstedc('V', n, d, e, z, ldz, rdum, lwork, idum, liwork, info)

!     Make sure that there is enough workspace.
      lwork = max(1+3*n+2*n*lgn+4*n*n, nint(rdum(1)))
      liwork = max(6+6*n+5*n*lgn, idum(1))
      Allocate (work(lwork), iwork(liwork))

!     Read the upper or lower triangular part of the band matrix A
!     from data file

      If (uplo=='U') Then
        Do i = 1, n
          Read (nin, *)(ab(kd+1+i-j,j), j=i, min(n,i+kd))
        End Do
      Else If (uplo=='L') Then
        Do i = 1, n
          Read (nin, *)(ab(1+i-j,j), j=max(1,i-kd), i)
        End Do
      End If

!     Reduce A to tridiagonal form T = (Z**T)*A*Z, and form Z
      Call dsbtrd('V', uplo, n, kd, ab, ldab, d, e, z, ldz, work, info)

!     Calculate all the eigenvalues and eigenvectors of A,
!     from T and Z
      Call dstedc('V', n, d, e, z, ldz, work, lwork, iwork, liwork, info)

      If (info==0) Then

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
!               =0 for hard exit, =1 for quiet-soft, =-1 for noisy-soft
        ifail = 0
        Call nagf_file_print_matrix_real_gen('General', ' ', n, n, z, ldz, &
          'Eigenvectors', ifail)

      Else
        Write (nout, 110) 'Failure in DSTEDC. INFO = ', info
      End If

100   Format ((3X,8F8.4))
110   Format (1X, A, I10)
    End Program
