    Program zungbr_example

!     ZUNGBR Example Program Text

!     Copyright 2017, Numerical Algorithms Group Ltd. http://www.nag.com

!     .. Use Statements ..
      Use lapack_example_aux, Only: nagf_file_print_matrix_complex_gen_comp
      Use lapack_interfaces, Only: zbdsqr, zgebrd, zlacpy, zungbr
      Use lapack_precision, Only: dp
!     .. Implicit None Statement ..
      Implicit None
!     .. Parameters ..
      Integer, Parameter :: nin = 5, nout = 6
!     .. Local Scalars ..
      Integer :: i, ic, ifail, info, lda, ldc, ldu, ldvt, lwork, m, n
!     .. Local Arrays ..
      Complex (Kind=dp), Allocatable :: a(:, :), c(:, :), taup(:), tauq(:), &
        u(:, :), vt(:, :), work(:)
      Real (Kind=dp), Allocatable :: d(:), e(:), rwork(:)
      Character (1) :: clabs(1), rlabs(1)
!     .. Executable Statements ..
      Write (nout, *) 'ZUNGBR Example Program Results'
!     Skip heading in data file
      Read (nin, *)
      Do ic = 1, 2
        Read (nin, *) m, n
        lda = m
        ldc = n
        ldu = m
        ldvt = n
        lwork = 64*(m+n)
        Allocate (a(lda,n), c(ldc,n), taup(n), tauq(n), u(ldu,n), vt(ldvt,n), &
          work(lwork), d(n), e(n-1), rwork(4*n-4))

!       Read A from data file
        Read (nin, *)(a(i,1:n), i=1, m)

!       Reduce A to bidiagonal form
        Call zgebrd(m, n, a, lda, d, e, tauq, taup, work, lwork, info)

        If (m>=n) Then

!         Copy A to VT and U
          Call zlacpy('Upper', n, n, a, lda, vt, ldvt)
          Call zlacpy('Lower', m, n, a, lda, u, ldu)

!         Form P**H explicitly, storing the result in VT
          Call zungbr('P', n, n, m, vt, ldvt, taup, work, lwork, info)

!         Form Q explicitly, storing the result in U
          Call zungbr('Q', m, n, n, u, ldu, tauq, work, lwork, info)

!         Compute the SVD of A
          Call zbdsqr('Upper', n, n, m, 0, d, e, vt, ldvt, u, ldu, c, ldc, &
            rwork, info)

!         Print singular values, left & right singular vectors

          Write (nout, *)
          Write (nout, *) 'Example 1: singular values'
          Write (nout, 100) d(1:n)
          Write (nout, *)
          Flush (nout)

!         ifail: behaviour on error exit
!                =0 for hard exit, =1 for quiet-soft, =-1 for noisy-soft
          ifail = 0
          Call nagf_file_print_matrix_complex_gen_comp('General', ' ', n, n, &
            vt, ldvt, 'Bracketed', 'F7.4', &
            'Example 1: right singular vectors, by row', 'Integer', rlabs, &
            'Integer', clabs, 80, 0, ifail)

          Write (nout, *)
          Flush (nout)

          Call nagf_file_print_matrix_complex_gen_comp('General', ' ', m, n, &
            u, ldu, 'Bracketed', 'F7.4', &
            'Example 1: left singular vectors, by column', 'Integer', rlabs, &
            'Integer', clabs, 80, 0, ifail)

        Else

!         Copy A to VT and U
          Call zlacpy('Upper', m, n, a, lda, vt, ldvt)
          Call zlacpy('Lower', m, m, a, lda, u, ldu)

!         Form P**H explicitly, storing the result in VT
          Call zungbr('P', m, n, m, vt, ldvt, taup, work, lwork, info)

!         Form Q explicitly, storing the result in U
          Call zungbr('Q', m, m, n, u, ldu, tauq, work, lwork, info)

!         Compute the SVD of A
          Call zbdsqr('Lower', m, n, m, 0, d, e, vt, ldvt, u, ldu, c, ldc, &
            rwork, info)

!         Print singular values, left & right singular vectors

          Write (nout, *)
          Write (nout, *) 'Example 2: singular values'
          Write (nout, 100) d(1:m)
          Write (nout, *)
          Flush (nout)

          ifail = 0
          Call nagf_file_print_matrix_complex_gen_comp('General', ' ', m, n, &
            vt, ldvt, 'Bracketed', 'F7.4', &
            'Example 2: right singular vectors, by row', 'Integer', rlabs, &
            'Integer', clabs, 80, 0, ifail)

          Write (nout, *)
          Flush (nout)

          Call nagf_file_print_matrix_complex_gen_comp('General', ' ', m, m, &
            u, ldu, 'Bracketed', 'F7.4', &
            'Example 2: left singular vectors, by column', 'Integer', rlabs, &
            'Integer', clabs, 80, 0, ifail)

        End If
        Deallocate (a, c, taup, tauq, u, vt, work, d, e, rwork)
      End Do

100   Format (8X, 4(F7.4,11X,:))
    End Program
