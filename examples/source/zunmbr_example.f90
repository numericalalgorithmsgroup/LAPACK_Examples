    Program zunmbr_example

!     ZUNMBR Example Program Text

!     Copyright (c) 2018, Numerical Algorithms Group (NAG Ltd.)
!     For licence see
!       https://github.com/numericalalgorithmsgroup/LAPACK_Examples/blob/master/LICENCE.md

!     .. Use Statements ..
      Use lapack_example_aux, Only: nagf_blas_zmload, &
        nagf_file_print_matrix_complex_gen_comp
      Use lapack_interfaces, Only: zgebrd, zgelqf, zgeqrf, zlacpy, zunglq, &
        zungqr, zunmbr
      Use lapack_precision, Only: dp
!     .. Implicit None Statement ..
      Implicit None
!     .. Parameters ..
      Complex (Kind=dp), Parameter :: zero = (0.0E0_dp, 0.0E0_dp)
      Integer, Parameter :: nin = 5, nout = 6
!     .. Local Scalars ..
      Integer :: i, ic, ifail, info, lda, ldph, ldu, lwork, m, n
!     .. Local Arrays ..
      Complex (Kind=dp), Allocatable :: a(:, :), ph(:, :), tau(:), taup(:), &
        tauq(:), u(:, :), work(:)
      Real (Kind=dp), Allocatable :: d(:), e(:)
      Character (1) :: clabs(1), rlabs(1)
!     .. Executable Statements ..
      Write (nout, *) 'ZUNMBR Example Program Results'
!     Skip heading in data file
      Read (nin, *)
      Do ic = 1, 2
        Read (nin, *) m, n
        lda = m
        ldph = n
        ldu = m
        lwork = 64*(m+n)
        Allocate (a(lda,n), ph(ldph,n), tau(n), taup(n), tauq(n), u(ldu,n), &
          work(lwork), d(n), e(n-1))

!       Read A from data file

        Read (nin, *)(a(i,1:n), i=1, m)

        If (m>=n) Then

!         Compute the QR factorization of A
          Call zgeqrf(m, n, a, lda, tau, work, lwork, info)

!         Copy A to U
          Call zlacpy('Lower', m, n, a, lda, u, ldu)

!         Form Q explicitly, storing the result in U
          Call zungqr(m, n, n, u, ldu, tau, work, lwork, info)

!         Copy R to PH (used as workspace)
          Call zlacpy('Upper', n, n, a, lda, ph, ldph)

!         Set the strictly lower triangular part of R to zero
          Call nagf_blas_zmload('Lower', n-1, n-1, zero, zero, ph(2,1), ldph)

!         Bidiagonalize R
          Call zgebrd(n, n, ph, ldph, d, e, tauq, taup, work, lwork, info)

!         Update Q, storing the result in U
          Call zunmbr('Q', 'Right', 'No transpose', m, n, n, ph, ldph, tauq, &
            u, ldu, work, lwork, info)

!         Print bidiagonal form and matrix Q

          Write (nout, *)
          Write (nout, *) 'Example 1: bidiagonal matrix B'
          Write (nout, *) 'Diagonal'
          Write (nout, 100) d(1:n)
          Write (nout, *) 'Superdiagonal'
          Write (nout, 100) e(1:n-1)
          Write (nout, *)
          Flush (nout)

!         ifail: behaviour on error exit
!                =0 for hard exit, =1 for quiet-soft, =-1 for noisy-soft
          ifail = 0
          Call nagf_file_print_matrix_complex_gen_comp('General', ' ', m, n, &
            u, ldu, 'Bracketed', 'F7.4', 'Example 1: matrix Q', 'Integer', &
            rlabs, 'Integer', clabs, 80, 0, ifail)

        Else

!         Compute the LQ factorization of A
          Call zgelqf(m, n, a, lda, tau, work, lwork, info)

!         Copy A to PH
          Call zlacpy('Upper', m, n, a, lda, ph, ldph)

!         Form Q explicitly, storing the result in PH
          Call zunglq(n, n, m, ph, ldph, tau, work, lwork, info)

!         Copy L to U (used as workspace)
          Call zlacpy('Lower', m, m, a, lda, u, ldu)

!         Set the strictly upper triangular part of L to zero
          Call nagf_blas_zmload('Upper', m-1, m-1, zero, zero, u(1,2), ldu)

!         Bidiagonalize L
          Call zgebrd(m, m, u, ldu, d, e, tauq, taup, work, lwork, info)

!         Update P**H, storing the result in PH
          Call zunmbr('P', 'Left', 'Conjugate transpose', m, n, m, u, ldu, &
            taup, ph, ldph, work, lwork, info)

!         Print bidiagonal form and matrix P**H

          Write (nout, *)
          Write (nout, *) 'Example 2: bidiagonal matrix B'
          Write (nout, *) 'Diagonal'
          Write (nout, 100) d(1:m)
          Write (nout, *) 'Superdiagonal'
          Write (nout, 100) e(1:m-1)
          Write (nout, *)
          Flush (nout)

          ifail = 0
          Call nagf_file_print_matrix_complex_gen_comp('General', ' ', m, n, &
            ph, ldph, 'Bracketed', 'F7.4', 'Example 2: matrix P**H', &
            'Integer', rlabs, 'Integer', clabs, 80, 0, ifail)

        End If
        Deallocate (a, ph, tau, taup, tauq, u, work, d, e)
      End Do

100   Format (3X, (8F8.4))
    End Program
