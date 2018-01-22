    Program zstedc_example

!     ZSTEDC Example Program Text

!     Copyright 2017, Numerical Algorithms Group Ltd. http://www.nag.com

!     .. Use Statements ..
      Use blas_interfaces, Only: dznrm2
      Use lapack_example_aux, Only: nagf_file_print_matrix_complex_gen_comp
      Use lapack_interfaces, Only: zhbtrd, zstedc
      Use lapack_precision, Only: dp
!     .. Implicit None Statement ..
      Implicit None
!     .. Parameters ..
      Integer, Parameter :: nin = 5, nout = 6
      Character (1), Parameter :: uplo = 'U'
!     .. Local Scalars ..
      Complex (Kind=dp) :: scal
      Integer :: i, ifail, info, j, k, kd, ldab, ldz, lgn, liwork, lrwork, &
        lwork, n
!     .. Local Arrays ..
      Complex (Kind=dp), Allocatable :: ab(:, :), work(:), z(:, :)
      Complex (Kind=dp) :: cdum(1)
      Real (Kind=dp), Allocatable :: d(:), e(:), rwork(:)
      Real (Kind=dp) :: rdum(1)
      Integer :: idum(1)
      Integer, Allocatable :: iwork(:)
      Character (1) :: clabs(1), rlabs(1)
!     .. Intrinsic Procedures ..
      Intrinsic :: abs, ceiling, conjg, log, max, maxloc, min, nint, real
!     .. Executable Statements ..
      Write (nout, *) 'ZSTEDC Example Program Results'
      Write (nout, *)
!     Skip heading in data file
      Read (nin, *)
      Read (nin, *) n, kd
      ldab = kd + 1
      ldz = n
      lgn = ceiling(log(real(n,kind=dp))/log(2.0_dp))
      Allocate (ab(ldab,n), z(ldz,n), d(n), e(n-1))

!     Use routine workspace query to get optimal workspace.
      lwork = -1
      lrwork = -1
      liwork = -1
      Call zstedc('V', n, d, e, z, ldz, cdum, lwork, rdum, lrwork, idum, &
        liwork, info)

!     Make sure that there is enough workspace.
      lwork = max(n*n, nint(real(cdum(1))))
      lrwork = max(1+3*n+2*n*lgn+4*n*n, nint(rdum(1)))
      liwork = max(6+6*n+5*n*lgn, idum(1))
      Allocate (work(lwork), rwork(lrwork), iwork(liwork))

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

      Call zhbtrd('V', uplo, n, kd, ab, ldab, d, e, z, ldz, work, info)

!     Calculate all the eigenvalues and eigenvectors of A,
!     from T and Z

      Call zstedc('V', n, d, e, z, ldz, work, lwork, rwork, lrwork, iwork, &
        liwork, info)

      If (info==0) Then

!       Print eigenvalues and eigenvectors

        Write (nout, *) 'Eigenvalues'
        Write (nout, 100) d(1:n)

        Write (nout, *)
        Flush (nout)

!       Normalize the eigenvectors, largest element real
        Do i = 1, n
          rwork(1:n) = abs(z(1:n,i))
          k = maxloc(rwork(1:n), 1)
          scal = conjg(z(k,i))/abs(z(k,i))/dznrm2(n, z(1,i), 1)
          z(1:n, i) = z(1:n, i)*scal
        End Do

!       ifail: behaviour on error exit
!              =0 for hard exit, =1 for quiet-soft, =-1 for noisy-soft
        ifail = 0
        Call nagf_file_print_matrix_complex_gen_comp('General', ' ', n, n, z, &
          ldz, 'Bracketed', 'F7.4', 'Eigenvectors', 'Integer', rlabs, &
          'Integer', clabs, 80, 0, ifail)

      Else
        Write (nout, 110) 'Failure in ZSTEDC. INFO = ', info
      End If

100   Format (4X, F8.4, 3(10X,F8.4))
110   Format (1X, A, I10)
    End Program
