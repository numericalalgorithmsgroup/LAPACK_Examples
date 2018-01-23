    Program zgbtrf_example

!     ZGBTRF Example Program Text

!     Copyright (c) 2018, Numerical Algorithms Group (NAG Ltd.)
!     For licence see
!       https://github.com/numericalalgorithmsgroup/LAPACK_Examples/blob/master/LICENCE.md

!     .. Use Statements ..
      Use lapack_example_aux, Only: nagf_file_print_matrix_complex_band_comp
      Use lapack_interfaces, Only: zgbtrf
      Use lapack_precision, Only: dp
!     .. Implicit None Statement ..
      Implicit None
!     .. Parameters ..
      Integer, Parameter :: nin = 5, nout = 6
!     .. Local Scalars ..
      Integer :: i, ifail, info, j, k, kl, ku, ldab, m, n
!     .. Local Arrays ..
      Complex (Kind=dp), Allocatable :: ab(:, :)
      Integer, Allocatable :: ipiv(:)
      Character (1) :: clabs(1), rlabs(1)
!     .. Intrinsic Procedures ..
      Intrinsic :: max, min
!     .. Executable Statements ..
      Write (nout, *) 'ZGBTRF Example Program Results'
!     Skip heading in data file
      Read (nin, *)
      Read (nin, *) m, n, kl, ku
      ldab = 2*kl + ku + 1
      Allocate (ab(ldab,n), ipiv(n))

!     Read A from data file

      k = kl + ku + 1
      Read (nin, *)((ab(k+i-j,j),j=max(i-kl,1),min(i+ku,n)), i=1, m)

!     Factorize A
      Call zgbtrf(m, n, kl, ku, ab, ldab, ipiv, info)

!     Print details of factorization

      Write (nout, *)
      Flush (nout)

!     ifail: behaviour on error exit
!             =0 for hard exit, =1 for quiet-soft, =-1 for noisy-soft
      ifail = 0
      Call nagf_file_print_matrix_complex_band_comp(m, n, kl, kl+ku, ab, ldab, &
        'Bracketed', 'F7.4', 'Details of factorization', 'Integer', rlabs, &
        'Integer', clabs, 80, 0, ifail)

!     Print pivot indices

      Write (nout, *)
      Write (nout, *) 'IPIV'
      Write (nout, 100) ipiv(1:min(m,n))

      If (info/=0) Then
        Write (nout, *) 'The factor U is singular'
      End If

100   Format ((1X,I12,3I18))
    End Program
