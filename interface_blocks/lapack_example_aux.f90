    Module lapack_example_aux
! Interface module generated on 2017-01-31 at 10:58:02 +0000.
      Interface
        Function nagf_blas_dpyth(a, b)
          Use lapack_precision, Only: dp
          Real (dp) :: a, b, nagf_blas_dpyth
        End Function
        Function p01abf(ifail, ierror, srname, nrec, rec)
          Integer :: ierror, ifail, nrec, p01abf
          Character (*) :: rec(*), srname
        End Function
        Subroutine x04aafn(iflag, nerr)
          Integer :: iflag, nerr
        End Subroutine
        Subroutine x04abfn(iflag, nadv)
          Integer :: iflag, nadv
        End Subroutine
        Subroutine x04bafn(nout, rec)
          Integer :: nout
          Character (*) :: rec
        End Subroutine
        Subroutine nagf_file_print_matrix_real_gen(matrix, diag, m, n, a, lda, &
          title, ifail)
          Use lapack_precision, Only: dp
          Character (1) :: diag, matrix
          Integer :: ifail, lda, m, n
          Character (*) :: title
          Real (dp) :: a(lda, *)
        End Subroutine
        Subroutine x04cafn(matrix, diag, m, n, a, lda, title, nout, errbuf, &
          ifail)
          Use lapack_precision, Only: dp
          Character (1) :: diag, matrix
          Character (200) :: errbuf
          Integer :: ifail, lda, m, n, nout
          Character (*) :: title
          Real (dp) :: a(lda, *)
        End Subroutine
        Subroutine nagf_file_print_matrix_real_gen_comp(matrix, diag, m, n, a, &
          lda, form, title, labrow, rlabs, labcol, clabs, ncols, indent, &
          ifail)
          Use lapack_precision, Only: dp
          Character (*) :: clabs(*), form, rlabs(*), title
          Character (1) :: diag, labcol, labrow, matrix
          Integer :: ifail, indent, lda, m, n, ncols
          Real (dp) :: a(lda, *)
        End Subroutine
        Subroutine x04cbfn(matrix, diag, m, n, a, lda, form, title, labrow, &
          rlabs, labcol, clabs, ncols, indent, nout, errbuf, ifail)
          Use lapack_precision, Only: dp
          Character (*) :: clabs(*), form, rlabs(*), title
          Character (1) :: diag, labcol, labrow, matrix
          Character (200) :: errbuf
          Integer :: ifail, indent, lda, m, n, ncols, nout
          Real (dp) :: a(lda, *)
        End Subroutine
        Subroutine x04cbzn(str, start, finish)
          Integer :: finish, start
          Character (*) :: str
        End Subroutine
        Subroutine nagf_file_print_matrix_real_packed(uplo, diag, n, a, title, &
          ifail)
          Use lapack_precision, Only: dp
          Real (dp) :: a(*)
          Character (1) :: diag, uplo
          Integer :: ifail, n
          Character (*) :: title
        End Subroutine
        Subroutine x04ccfn(uplo, diag, n, a, title, errbuf, ifail)
          Use lapack_precision, Only: dp
          Real (dp) :: a(*)
          Character (1) :: diag, uplo
          Character (200) :: errbuf
          Integer :: ifail, n
          Character (*) :: title
        End Subroutine
        Subroutine x04cdfn(uplo, diag, n, a, form, title, labrow, rlabs, &
          labcol, clabs, ncols, indent, errbuf, ifail)
          Use lapack_precision, Only: dp
          Real (dp) :: a(*)
          Character (*) :: clabs(*), form, rlabs(*), title
          Character (1) :: diag, labcol, labrow, uplo
          Character (200) :: errbuf
          Integer :: ifail, indent, n, ncols
        End Subroutine
        Subroutine nagf_file_print_matrix_real_band(m, n, kl, ku, a, lda, &
          title, ifail)
          Use lapack_precision, Only: dp
          Integer :: ifail, kl, ku, lda, m, n
          Character (*) :: title
          Real (dp) :: a(lda, *)
        End Subroutine
        Subroutine x04cefn(m, n, kl, ku, a, lda, title, errbuf, ifail)
          Use lapack_precision, Only: dp
          Character (200) :: errbuf
          Integer :: ifail, kl, ku, lda, m, n
          Character (*) :: title
          Real (dp) :: a(lda, *)
        End Subroutine
        Subroutine x04cffn(m, n, kl, ku, a, lda, form, title, labrow, rlabs, &
          labcol, clabs, ncols, indent, errbuf, ifail)
          Use lapack_precision, Only: dp
          Character (*) :: clabs(*), form, rlabs(*), title
          Character (200) :: errbuf
          Integer :: ifail, indent, kl, ku, lda, m, n, ncols
          Character (1) :: labcol, labrow
          Real (dp) :: a(lda, *)
        End Subroutine
        Subroutine nagf_file_print_matrix_complex_gen(matrix, diag, m, n, a, &
          lda, title, ifail)
          Use lapack_precision, Only: dp
          Character (1) :: diag, matrix
          Integer :: ifail, lda, m, n
          Character (*) :: title
          Complex (dp) :: a(lda, *)
        End Subroutine
        Subroutine x04dafn(matrix, diag, m, n, a, lda, title, nout, errbuf, &
          ifail)
          Use lapack_precision, Only: dp
          Character (1) :: diag, matrix
          Character (200) :: errbuf
          Integer :: ifail, lda, m, n, nout
          Character (*) :: title
          Complex (dp) :: a(lda, *)
        End Subroutine
        Subroutine nagf_file_print_matrix_complex_gen_comp(matrix, diag, m, n, &
          a, lda, usefrm, form, title, labrow, rlabs, labcol, clabs, ncols, &
          indent, ifail)
          Use lapack_precision, Only: dp
          Character (*) :: clabs(*), form, rlabs(*), title
          Character (1) :: diag, labcol, labrow, matrix, usefrm
          Integer :: ifail, indent, lda, m, n, ncols
          Complex (dp) :: a(lda, *)
        End Subroutine
        Subroutine x04dbfn(matrix, diag, m, n, a, lda, usefrm, form, title, &
          labrow, rlabs, labcol, clabs, ncols, indent, nout, errbuf, ifail)
          Use lapack_precision, Only: dp
          Character (*) :: clabs(*), form, rlabs(*), title
          Character (1) :: diag, labcol, labrow, matrix, usefrm
          Character (200) :: errbuf
          Integer :: ifail, indent, lda, m, n, ncols, nout
          Complex (dp) :: a(lda, *)
        End Subroutine
        Subroutine nagf_file_print_matrix_complex_packed_comp(uplo, diag, n, &
          a, usefrm, form, title, labrow, rlabs, labcol, clabs, ncols, indent, &
          ifail)
          Use lapack_precision, Only: dp
          Complex (dp) :: a(*)
          Character (*) :: clabs(*), form, rlabs(*), title
          Character (1) :: diag, labcol, labrow, uplo, usefrm
          Integer :: ifail, indent, n, ncols
        End Subroutine
        Subroutine x04ddfn(uplo, diag, n, a, usefrm, form, title, labrow, &
          rlabs, labcol, clabs, ncols, indent, errbuf, ifail)
          Use lapack_precision, Only: dp
          Complex (dp) :: a(*)
          Character (*) :: clabs(*), form, rlabs(*), title
          Character (1) :: diag, labcol, labrow, uplo, usefrm
          Character (200) :: errbuf
          Integer :: ifail, indent, n, ncols
        End Subroutine
        Subroutine nagf_file_print_matrix_complex_band_comp(m, n, kl, ku, a, &
          lda, usefrm, form, title, labrow, rlabs, labcol, clabs, ncols, &
          indent, ifail)
          Use lapack_precision, Only: dp
          Character (*) :: clabs(*), form, rlabs(*), title
          Integer :: ifail, indent, kl, ku, lda, m, n, ncols
          Character (1) :: labcol, labrow, usefrm
          Complex (dp) :: a(lda, *)
        End Subroutine
        Subroutine x04dffn(m, n, kl, ku, a, lda, usefrm, form, title, labrow, &
          rlabs, labcol, clabs, ncols, indent, errbuf, ifail)
          Use lapack_precision, Only: dp
          Character (*) :: clabs(*), form, rlabs(*), title
          Character (200) :: errbuf
          Integer :: ifail, indent, kl, ku, lda, m, n, ncols
          Character (1) :: labcol, labrow, usefrm
          Complex (dp) :: a(lda, *)
        End Subroutine
        Subroutine nagf_file_print_matrix_complex_band(m, n, kl, ku, a, lda, &
          title, ifail)
          Use lapack_precision, Only: dp
          Implicit None
          Integer, Intent (Out) :: ifail
          Integer, Intent (In) :: kl, ku, lda, m, n
          Character (*), Intent (In) :: title
          Complex (dp), Intent (In) :: a(lda, *)
        End Subroutine
        Subroutine nagf_blas_damax_val(n, x, incx, k, r)
          Use lapack_precision, Only: dp
          Implicit None
          Intrinsic :: abs
          Integer, Intent (In) :: incx, n
          Integer, Intent (Out) :: k
          Real (dp), Intent (Out) :: r
          Real (dp), Intent (In) :: x(1+(n-1)*abs(incx))
        End Subroutine
        Subroutine nagf_blas_zmload(matrix, m, n, con, diag, a, lda)
          Use lapack_precision, Only: dp
          Implicit None
          Complex (dp), Intent (In) :: con, diag
          Integer, Intent (In) :: lda, m, n
          Character (1), Intent (In) :: matrix
          Complex (dp), Intent (Inout) :: a(lda, *)
        End Subroutine
        Function p01abu(rec, maxlen, append_null)
          Implicit None
          Logical, Intent (In) :: append_null
          Integer, Intent (In) :: maxlen
          Character (81) :: p01abu
          Character (*), Intent (In) :: rec
        End Function
        Subroutine nagf_sort_realvec_rank(rv, m1, m2, order, irank, ifail)
          Use lapack_precision, Only: dp
          Integer :: ifail, m1, m2, irank(m2)
          Character (1) :: order
          Real (dp) :: rv(m2)
        End Subroutine
        Subroutine nagf_sort_cmplxvec_rank_rearrange(cv, m1, m2, irank, ifail)
          Use lapack_precision, Only: dp
          Integer :: ifail, m1, m2, irank(m2)
          Complex (dp) :: cv(m2)
        End Subroutine
        Subroutine nagf_sort_realmat_rank_rows(rm, ldm, m1, m2, n1, n2, order, &
          irank, ifail)
          Use lapack_precision, Only: dp
          Integer :: ifail, ldm, m1, m2, n1, n2, irank(m2)
          Character (1) :: order
          Real (dp) :: rm(ldm, n2)
        End Subroutine
        Subroutine nagf_sort_realvec_rank_rearrange(rv, m1, m2, irank, ifail)
          Use lapack_precision, Only: dp
          Integer :: ifail, m1, m2, irank(m2)
          Real (dp) :: rv(m2)
        End Subroutine
        Subroutine nagf_file_print_matrix_integer_comp(matrix, diag, m, n, a, &
          lda, form, title, labrow, rlabs, labcol, clabs, ncols, indent, &
          ifail)
          Character (*) :: clabs(*), form, rlabs(*), title
          Character (1) :: diag, labcol, labrow, matrix
          Integer :: ifail, indent, lda, m, n, ncols, a(lda, *)
        End Subroutine
        Subroutine x04ebfn(matrix, diag, m, n, a, lda, form, title, labrow, &
          rlabs, labcol, clabs, ncols, indent, errbuf, ifail)
          Character (*) :: clabs(*), form, rlabs(*), title
          Character (1) :: diag, labcol, labrow, matrix
          Character (200) :: errbuf
          Integer :: ifail, indent, lda, m, n, ncols, a(lda, *)
        End Subroutine
        Subroutine nagf_blas_ddscl(n, d, incd, x, incx)
          Use lapack_precision, Only: dp
          Implicit None
          Real (dp), Intent (In) :: d(*)
          Integer, Intent (In) :: incd, incx, n
          Real (dp), Intent (Inout) :: x(*)
        End Subroutine
        Subroutine nagf_blas_zddscl(n, d, incd, x, incx)
          Use lapack_precision, Only: dp
          Implicit None
          Real (dp), Intent (In) :: d(*)
          Integer, Intent (In) :: incd, incx, n
          Complex (dp), Intent (Inout) :: x(*)
        End Subroutine
      End Interface
    End Module
