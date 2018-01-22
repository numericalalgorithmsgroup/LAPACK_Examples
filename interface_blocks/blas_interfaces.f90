    Module blas_interfaces

!     NAG COPYRIGHT 2017.

!     .. Implicit None Statement ..
      Implicit None
!     .. Interface Blocks ..
      Interface
        Function dasum(n, x, incx)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Function Return Value ..
          Real (Kind=dp) :: dasum
!         .. Scalar Arguments ..
          Integer, Intent (In) :: incx, n
!         .. Array Arguments ..
          Real (Kind=dp), Intent (In) :: x(*)
        End Function
        Subroutine daxpy(n, alpha, x, incx, y, incy)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Scalar Arguments ..
          Real (Kind=dp), Intent (In) :: alpha
          Integer, Intent (In) :: incx, incy, n
!         .. Array Arguments ..
          Real (Kind=dp), Intent (In) :: x(*)
          Real (Kind=dp), Intent (Inout) :: y(*)
        End Subroutine
        Subroutine daxpyi(nz, a, x, indx, y)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Scalar Arguments ..
          Real (Kind=dp), Intent (In) :: a
          Integer, Intent (In) :: nz
!         .. Array Arguments ..
          Real (Kind=dp), Intent (In) :: x(*)
          Real (Kind=dp), Intent (Inout) :: y(*)
          Integer, Intent (In) :: indx(*)
        End Subroutine
        Subroutine dcopy(n, x, incx, y, incy)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Scalar Arguments ..
          Integer, Intent (In) :: incx, incy, n
!         .. Array Arguments ..
          Real (Kind=dp), Intent (In) :: x(*)
          Real (Kind=dp), Intent (Inout) :: y(*)
        End Subroutine
        Function ddot(n, x, incx, y, incy)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Function Return Value ..
          Real (Kind=dp) :: ddot
!         .. Scalar Arguments ..
          Integer, Intent (In) :: incx, incy, n
!         .. Array Arguments ..
          Real (Kind=dp), Intent (In) :: x(*), y(*)
        End Function
        Function ddoti(nz, x, indx, y)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Function Return Value ..
          Real (Kind=dp) :: ddoti
!         .. Scalar Arguments ..
          Integer, Intent (In) :: nz
!         .. Array Arguments ..
          Real (Kind=dp), Intent (In) :: x(*), y(*)
          Integer, Intent (In) :: indx(*)
        End Function
        Subroutine dgbmv(trans, m, n, kl, ku, alpha, a, lda, x, incx, beta, y, &
          incy)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Scalar Arguments ..
          Real (Kind=dp), Intent (In) :: alpha, beta
          Integer, Intent (In) :: incx, incy, kl, ku, lda, m, n
          Character (1), Intent (In) :: trans
!         .. Array Arguments ..
          Real (Kind=dp), Intent (In) :: a(lda, *), x(*)
          Real (Kind=dp), Intent (Inout) :: y(*)
        End Subroutine
        Subroutine dgemm(transa, transb, m, n, k, alpha, a, lda, b, ldb, beta, &
          c, ldc)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Scalar Arguments ..
          Real (Kind=dp), Intent (In) :: alpha, beta
          Integer, Intent (In) :: k, lda, ldb, ldc, m, n
          Character (1), Intent (In) :: transa, transb
!         .. Array Arguments ..
          Real (Kind=dp), Intent (In) :: a(lda, *), b(ldb, *)
          Real (Kind=dp), Intent (Inout) :: c(ldc, *)
        End Subroutine
        Subroutine dgemv(trans, m, n, alpha, a, lda, x, incx, beta, y, incy)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Scalar Arguments ..
          Real (Kind=dp), Intent (In) :: alpha, beta
          Integer, Intent (In) :: incx, incy, lda, m, n
          Character (1), Intent (In) :: trans
!         .. Array Arguments ..
          Real (Kind=dp), Intent (In) :: a(lda, *), x(*)
          Real (Kind=dp), Intent (Inout) :: y(*)
        End Subroutine
        Subroutine dger(m, n, alpha, x, incx, y, incy, a, lda)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Scalar Arguments ..
          Real (Kind=dp), Intent (In) :: alpha
          Integer, Intent (In) :: incx, incy, lda, m, n
!         .. Array Arguments ..
          Real (Kind=dp), Intent (Inout) :: a(lda, *)
          Real (Kind=dp), Intent (In) :: x(*), y(*)
        End Subroutine
        Subroutine dgthr(nz, y, x, indx)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Scalar Arguments ..
          Integer, Intent (In) :: nz
!         .. Array Arguments ..
          Real (Kind=dp), Intent (Inout) :: x(*)
          Real (Kind=dp), Intent (In) :: y(*)
          Integer, Intent (In) :: indx(*)
        End Subroutine
        Subroutine dgthrz(nz, y, x, indx)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Scalar Arguments ..
          Integer, Intent (In) :: nz
!         .. Array Arguments ..
          Real (Kind=dp), Intent (Inout) :: x(*), y(*)
          Integer, Intent (In) :: indx(*)
        End Subroutine
        Function dnrm2(n, x, incx)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Function Return Value ..
          Real (Kind=dp) :: dnrm2
!         .. Scalar Arguments ..
          Integer, Intent (In) :: incx, n
!         .. Array Arguments ..
          Real (Kind=dp), Intent (In) :: x(*)
        End Function
        Subroutine drot(n, x, incx, y, incy, c, s)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Scalar Arguments ..
          Real (Kind=dp), Intent (In) :: c, s
          Integer, Intent (In) :: incx, incy, n
!         .. Array Arguments ..
          Real (Kind=dp), Intent (Inout) :: x(*), y(*)
        End Subroutine
        Subroutine drotg(a, b, c, s)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Scalar Arguments ..
          Real (Kind=dp), Intent (Inout) :: a, b
          Real (Kind=dp), Intent (Out) :: c, s
        End Subroutine
        Subroutine droti(nz, x, indx, y, c, s)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Scalar Arguments ..
          Real (Kind=dp), Intent (In) :: c, s
          Integer, Intent (In) :: nz
!         .. Array Arguments ..
          Real (Kind=dp), Intent (Inout) :: x(*), y(*)
          Integer, Intent (In) :: indx(*)
        End Subroutine
        Subroutine dsbmv(uplo, n, k, alpha, a, lda, x, incx, beta, y, incy)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Scalar Arguments ..
          Real (Kind=dp), Intent (In) :: alpha, beta
          Integer, Intent (In) :: incx, incy, k, lda, n
          Character (1), Intent (In) :: uplo
!         .. Array Arguments ..
          Real (Kind=dp), Intent (In) :: a(lda, *), x(*)
          Real (Kind=dp), Intent (Inout) :: y(*)
        End Subroutine
        Subroutine dscal(n, alpha, x, incx)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Scalar Arguments ..
          Real (Kind=dp), Intent (In) :: alpha
          Integer, Intent (In) :: incx, n
!         .. Array Arguments ..
          Real (Kind=dp), Intent (Inout) :: x(*)
        End Subroutine
        Subroutine dsctr(nz, x, indx, y)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Scalar Arguments ..
          Integer, Intent (In) :: nz
!         .. Array Arguments ..
          Real (Kind=dp), Intent (In) :: x(*)
          Real (Kind=dp), Intent (Inout) :: y(*)
          Integer, Intent (In) :: indx(*)
        End Subroutine
        Subroutine dspmv(uplo, n, alpha, ap, x, incx, beta, y, incy)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Scalar Arguments ..
          Real (Kind=dp), Intent (In) :: alpha, beta
          Integer, Intent (In) :: incx, incy, n
          Character (1), Intent (In) :: uplo
!         .. Array Arguments ..
          Real (Kind=dp), Intent (In) :: ap(*), x(*)
          Real (Kind=dp), Intent (Inout) :: y(*)
        End Subroutine
        Subroutine dspr(uplo, n, alpha, x, incx, ap)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Scalar Arguments ..
          Real (Kind=dp), Intent (In) :: alpha
          Integer, Intent (In) :: incx, n
          Character (1), Intent (In) :: uplo
!         .. Array Arguments ..
          Real (Kind=dp), Intent (Inout) :: ap(*)
          Real (Kind=dp), Intent (In) :: x(*)
        End Subroutine
        Subroutine dspr2(uplo, n, alpha, x, incx, y, incy, ap)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Scalar Arguments ..
          Real (Kind=dp), Intent (In) :: alpha
          Integer, Intent (In) :: incx, incy, n
          Character (1), Intent (In) :: uplo
!         .. Array Arguments ..
          Real (Kind=dp), Intent (Inout) :: ap(*)
          Real (Kind=dp), Intent (In) :: x(*), y(*)
        End Subroutine
        Subroutine dswap(n, x, incx, y, incy)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Scalar Arguments ..
          Integer, Intent (In) :: incx, incy, n
!         .. Array Arguments ..
          Real (Kind=dp), Intent (Inout) :: x(*), y(*)
        End Subroutine
        Subroutine dsymm(side, uplo, m, n, alpha, a, lda, b, ldb, beta, c, &
          ldc)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Scalar Arguments ..
          Real (Kind=dp), Intent (In) :: alpha, beta
          Integer, Intent (In) :: lda, ldb, ldc, m, n
          Character (1), Intent (In) :: side, uplo
!         .. Array Arguments ..
          Real (Kind=dp), Intent (In) :: a(lda, *), b(ldb, *)
          Real (Kind=dp), Intent (Inout) :: c(ldc, *)
        End Subroutine
        Subroutine dsymv(uplo, n, alpha, a, lda, x, incx, beta, y, incy)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Scalar Arguments ..
          Real (Kind=dp), Intent (In) :: alpha, beta
          Integer, Intent (In) :: incx, incy, lda, n
          Character (1), Intent (In) :: uplo
!         .. Array Arguments ..
          Real (Kind=dp), Intent (In) :: a(lda, *), x(*)
          Real (Kind=dp), Intent (Inout) :: y(*)
        End Subroutine
        Subroutine dsyr(uplo, n, alpha, x, incx, a, lda)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Scalar Arguments ..
          Real (Kind=dp), Intent (In) :: alpha
          Integer, Intent (In) :: incx, lda, n
          Character (1), Intent (In) :: uplo
!         .. Array Arguments ..
          Real (Kind=dp), Intent (Inout) :: a(lda, *)
          Real (Kind=dp), Intent (In) :: x(*)
        End Subroutine
        Subroutine dsyr2(uplo, n, alpha, x, incx, y, incy, a, lda)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Scalar Arguments ..
          Real (Kind=dp), Intent (In) :: alpha
          Integer, Intent (In) :: incx, incy, lda, n
          Character (1), Intent (In) :: uplo
!         .. Array Arguments ..
          Real (Kind=dp), Intent (Inout) :: a(lda, *)
          Real (Kind=dp), Intent (In) :: x(*), y(*)
        End Subroutine
        Subroutine dsyr2k(uplo, trans, n, k, alpha, a, lda, b, ldb, beta, c, &
          ldc)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Scalar Arguments ..
          Real (Kind=dp), Intent (In) :: alpha, beta
          Integer, Intent (In) :: k, lda, ldb, ldc, n
          Character (1), Intent (In) :: trans, uplo
!         .. Array Arguments ..
          Real (Kind=dp), Intent (In) :: a(lda, *), b(ldb, *)
          Real (Kind=dp), Intent (Inout) :: c(ldc, *)
        End Subroutine
        Subroutine dsyrk(uplo, trans, n, k, alpha, a, lda, beta, c, ldc)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Scalar Arguments ..
          Real (Kind=dp), Intent (In) :: alpha, beta
          Integer, Intent (In) :: k, lda, ldc, n
          Character (1), Intent (In) :: trans, uplo
!         .. Array Arguments ..
          Real (Kind=dp), Intent (In) :: a(lda, *)
          Real (Kind=dp), Intent (Inout) :: c(ldc, *)
        End Subroutine
        Subroutine dtbmv(uplo, trans, diag, n, k, a, lda, x, incx)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Scalar Arguments ..
          Integer, Intent (In) :: incx, k, lda, n
          Character (1), Intent (In) :: diag, trans, uplo
!         .. Array Arguments ..
          Real (Kind=dp), Intent (In) :: a(lda, *)
          Real (Kind=dp), Intent (Inout) :: x(*)
        End Subroutine
        Subroutine dtbsv(uplo, trans, diag, n, k, a, lda, x, incx)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Scalar Arguments ..
          Integer, Intent (In) :: incx, k, lda, n
          Character (1), Intent (In) :: diag, trans, uplo
!         .. Array Arguments ..
          Real (Kind=dp), Intent (In) :: a(lda, *)
          Real (Kind=dp), Intent (Inout) :: x(*)
        End Subroutine
        Subroutine dtpmv(uplo, trans, diag, n, ap, x, incx)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Scalar Arguments ..
          Integer, Intent (In) :: incx, n
          Character (1), Intent (In) :: diag, trans, uplo
!         .. Array Arguments ..
          Real (Kind=dp), Intent (In) :: ap(*)
          Real (Kind=dp), Intent (Inout) :: x(*)
        End Subroutine
        Subroutine dtpsv(uplo, trans, diag, n, ap, x, incx)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Scalar Arguments ..
          Integer, Intent (In) :: incx, n
          Character (1), Intent (In) :: diag, trans, uplo
!         .. Array Arguments ..
          Real (Kind=dp), Intent (In) :: ap(*)
          Real (Kind=dp), Intent (Inout) :: x(*)
        End Subroutine
        Subroutine dtrmm(side, uplo, transa, diag, m, n, alpha, a, lda, b, &
          ldb)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Scalar Arguments ..
          Real (Kind=dp), Intent (In) :: alpha
          Integer, Intent (In) :: lda, ldb, m, n
          Character (1), Intent (In) :: diag, side, transa, uplo
!         .. Array Arguments ..
          Real (Kind=dp), Intent (In) :: a(lda, *)
          Real (Kind=dp), Intent (Inout) :: b(ldb, *)
        End Subroutine
        Subroutine dtrmv(uplo, trans, diag, n, a, lda, x, incx)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Scalar Arguments ..
          Integer, Intent (In) :: incx, lda, n
          Character (1), Intent (In) :: diag, trans, uplo
!         .. Array Arguments ..
          Real (Kind=dp), Intent (In) :: a(lda, *)
          Real (Kind=dp), Intent (Inout) :: x(*)
        End Subroutine
        Subroutine dtrsm(side, uplo, transa, diag, m, n, alpha, a, lda, b, &
          ldb)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Scalar Arguments ..
          Real (Kind=dp), Intent (In) :: alpha
          Integer, Intent (In) :: lda, ldb, m, n
          Character (1), Intent (In) :: diag, side, transa, uplo
!         .. Array Arguments ..
          Real (Kind=dp), Intent (In) :: a(lda, *)
          Real (Kind=dp), Intent (Inout) :: b(ldb, *)
        End Subroutine
        Subroutine dtrsv(uplo, trans, diag, n, a, lda, x, incx)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Scalar Arguments ..
          Integer, Intent (In) :: incx, lda, n
          Character (1), Intent (In) :: diag, trans, uplo
!         .. Array Arguments ..
          Real (Kind=dp), Intent (In) :: a(lda, *)
          Real (Kind=dp), Intent (Inout) :: x(*)
        End Subroutine
        Function dzasum(n, x, incx)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Function Return Value ..
          Real (Kind=dp) :: dzasum
!         .. Scalar Arguments ..
          Integer, Intent (In) :: incx, n
!         .. Array Arguments ..
          Complex (Kind=dp), Intent (In) :: x(*)
        End Function
        Function dznrm2(n, x, incx)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Function Return Value ..
          Real (Kind=dp) :: dznrm2
!         .. Scalar Arguments ..
          Integer, Intent (In) :: incx, n
!         .. Array Arguments ..
          Complex (Kind=dp), Intent (In) :: x(*)
        End Function
        Function idamax(n, x, incx)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Function Return Value ..
          Integer :: idamax
!         .. Scalar Arguments ..
          Integer, Intent (In) :: incx, n
!         .. Array Arguments ..
          Real (Kind=dp), Intent (In) :: x(*)
        End Function
        Function izamax(n, x, incx)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Function Return Value ..
          Integer :: izamax
!         .. Scalar Arguments ..
          Integer, Intent (In) :: incx, n
!         .. Array Arguments ..
          Complex (Kind=dp), Intent (In) :: x(*)
        End Function
        Subroutine zaxpy(n, alpha, x, incx, y, incy)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Scalar Arguments ..
          Complex (Kind=dp), Intent (In) :: alpha
          Integer, Intent (In) :: incx, incy, n
!         .. Array Arguments ..
          Complex (Kind=dp), Intent (In) :: x(*)
          Complex (Kind=dp), Intent (Inout) :: y(*)
        End Subroutine
        Subroutine zaxpyi(nz, a, x, indx, y)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Scalar Arguments ..
          Complex (Kind=dp), Intent (In) :: a
          Integer, Intent (In) :: nz
!         .. Array Arguments ..
          Complex (Kind=dp), Intent (In) :: x(*)
          Complex (Kind=dp), Intent (Inout) :: y(*)
          Integer, Intent (In) :: indx(*)
        End Subroutine
        Subroutine zcopy(n, x, incx, y, incy)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Scalar Arguments ..
          Integer, Intent (In) :: incx, incy, n
!         .. Array Arguments ..
          Complex (Kind=dp), Intent (In) :: x(*)
          Complex (Kind=dp), Intent (Inout) :: y(*)
        End Subroutine
        Function zdotc(n, x, incx, y, incy)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Function Return Value ..
          Complex (Kind=dp) :: zdotc
!         .. Scalar Arguments ..
          Integer, Intent (In) :: incx, incy, n
!         .. Array Arguments ..
          Complex (Kind=dp), Intent (In) :: x(*), y(*)
        End Function
        Function zdotci(nz, x, indx, y)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Function Return Value ..
          Complex (Kind=dp) :: zdotci
!         .. Scalar Arguments ..
          Integer, Intent (In) :: nz
!         .. Array Arguments ..
          Complex (Kind=dp), Intent (In) :: x(*), y(*)
          Integer, Intent (In) :: indx(*)
        End Function
        Function zdotu(n, x, incx, y, incy)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Function Return Value ..
          Complex (Kind=dp) :: zdotu
!         .. Scalar Arguments ..
          Integer, Intent (In) :: incx, incy, n
!         .. Array Arguments ..
          Complex (Kind=dp), Intent (In) :: x(*), y(*)
        End Function
        Function zdotui(nz, x, indx, y)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Function Return Value ..
          Complex (Kind=dp) :: zdotui
!         .. Scalar Arguments ..
          Integer, Intent (In) :: nz
!         .. Array Arguments ..
          Complex (Kind=dp), Intent (In) :: x(*), y(*)
          Integer, Intent (In) :: indx(*)
        End Function
        Subroutine zdrot(n, x, incx, y, incy, c, s)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Scalar Arguments ..
          Real (Kind=dp), Intent (In) :: c, s
          Integer, Intent (In) :: incx, incy, n
!         .. Array Arguments ..
          Complex (Kind=dp), Intent (Inout) :: x(*), y(*)
        End Subroutine
        Subroutine zdscal(n, alpha, x, incx)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Scalar Arguments ..
          Real (Kind=dp), Intent (In) :: alpha
          Integer, Intent (In) :: incx, n
!         .. Array Arguments ..
          Complex (Kind=dp), Intent (Inout) :: x(*)
        End Subroutine
        Subroutine zgbmv(trans, m, n, kl, ku, alpha, a, lda, x, incx, beta, y, &
          incy)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Scalar Arguments ..
          Complex (Kind=dp), Intent (In) :: alpha, beta
          Integer, Intent (In) :: incx, incy, kl, ku, lda, m, n
          Character (1), Intent (In) :: trans
!         .. Array Arguments ..
          Complex (Kind=dp), Intent (In) :: a(lda, *), x(*)
          Complex (Kind=dp), Intent (Inout) :: y(*)
        End Subroutine
        Subroutine zgemm(transa, transb, m, n, k, alpha, a, lda, b, ldb, beta, &
          c, ldc)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Scalar Arguments ..
          Complex (Kind=dp), Intent (In) :: alpha, beta
          Integer, Intent (In) :: k, lda, ldb, ldc, m, n
          Character (1), Intent (In) :: transa, transb
!         .. Array Arguments ..
          Complex (Kind=dp), Intent (In) :: a(lda, *), b(ldb, *)
          Complex (Kind=dp), Intent (Inout) :: c(ldc, *)
        End Subroutine
        Subroutine zgemv(trans, m, n, alpha, a, lda, x, incx, beta, y, incy)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Scalar Arguments ..
          Complex (Kind=dp), Intent (In) :: alpha, beta
          Integer, Intent (In) :: incx, incy, lda, m, n
          Character (1), Intent (In) :: trans
!         .. Array Arguments ..
          Complex (Kind=dp), Intent (In) :: a(lda, *), x(*)
          Complex (Kind=dp), Intent (Inout) :: y(*)
        End Subroutine
        Subroutine zgerc(m, n, alpha, x, incx, y, incy, a, lda)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Scalar Arguments ..
          Complex (Kind=dp), Intent (In) :: alpha
          Integer, Intent (In) :: incx, incy, lda, m, n
!         .. Array Arguments ..
          Complex (Kind=dp), Intent (Inout) :: a(lda, *)
          Complex (Kind=dp), Intent (In) :: x(*), y(*)
        End Subroutine
        Subroutine zgeru(m, n, alpha, x, incx, y, incy, a, lda)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Scalar Arguments ..
          Complex (Kind=dp), Intent (In) :: alpha
          Integer, Intent (In) :: incx, incy, lda, m, n
!         .. Array Arguments ..
          Complex (Kind=dp), Intent (Inout) :: a(lda, *)
          Complex (Kind=dp), Intent (In) :: x(*), y(*)
        End Subroutine
        Subroutine zgthr(nz, y, x, indx)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Scalar Arguments ..
          Integer, Intent (In) :: nz
!         .. Array Arguments ..
          Complex (Kind=dp), Intent (Inout) :: x(*)
          Complex (Kind=dp), Intent (In) :: y(*)
          Integer, Intent (In) :: indx(*)
        End Subroutine
        Subroutine zgthrz(nz, y, x, indx)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Scalar Arguments ..
          Integer, Intent (In) :: nz
!         .. Array Arguments ..
          Complex (Kind=dp), Intent (Inout) :: x(*), y(*)
          Integer, Intent (In) :: indx(*)
        End Subroutine
        Subroutine zhbmv(uplo, n, k, alpha, a, lda, x, incx, beta, y, incy)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Scalar Arguments ..
          Complex (Kind=dp), Intent (In) :: alpha, beta
          Integer, Intent (In) :: incx, incy, k, lda, n
          Character (1), Intent (In) :: uplo
!         .. Array Arguments ..
          Complex (Kind=dp), Intent (In) :: a(lda, *), x(*)
          Complex (Kind=dp), Intent (Inout) :: y(*)
        End Subroutine
        Subroutine zhemm(side, uplo, m, n, alpha, a, lda, b, ldb, beta, c, &
          ldc)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Scalar Arguments ..
          Complex (Kind=dp), Intent (In) :: alpha, beta
          Integer, Intent (In) :: lda, ldb, ldc, m, n
          Character (1), Intent (In) :: side, uplo
!         .. Array Arguments ..
          Complex (Kind=dp), Intent (In) :: a(lda, *), b(ldb, *)
          Complex (Kind=dp), Intent (Inout) :: c(ldc, *)
        End Subroutine
        Subroutine zhemv(uplo, n, alpha, a, lda, x, incx, beta, y, incy)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Scalar Arguments ..
          Complex (Kind=dp), Intent (In) :: alpha, beta
          Integer, Intent (In) :: incx, incy, lda, n
          Character (1), Intent (In) :: uplo
!         .. Array Arguments ..
          Complex (Kind=dp), Intent (In) :: a(lda, *), x(*)
          Complex (Kind=dp), Intent (Inout) :: y(*)
        End Subroutine
        Subroutine zher(uplo, n, alpha, x, incx, a, lda)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Scalar Arguments ..
          Real (Kind=dp), Intent (In) :: alpha
          Integer, Intent (In) :: incx, lda, n
          Character (1), Intent (In) :: uplo
!         .. Array Arguments ..
          Complex (Kind=dp), Intent (Inout) :: a(lda, *)
          Complex (Kind=dp), Intent (In) :: x(*)
        End Subroutine
        Subroutine zher2(uplo, n, alpha, x, incx, y, incy, a, lda)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Scalar Arguments ..
          Complex (Kind=dp), Intent (In) :: alpha
          Integer, Intent (In) :: incx, incy, lda, n
          Character (1), Intent (In) :: uplo
!         .. Array Arguments ..
          Complex (Kind=dp), Intent (Inout) :: a(lda, *)
          Complex (Kind=dp), Intent (In) :: x(*), y(*)
        End Subroutine
        Subroutine zher2k(uplo, trans, n, k, alpha, a, lda, b, ldb, beta, c, &
          ldc)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Scalar Arguments ..
          Complex (Kind=dp), Intent (In) :: alpha
          Real (Kind=dp), Intent (In) :: beta
          Integer, Intent (In) :: k, lda, ldb, ldc, n
          Character (1), Intent (In) :: trans, uplo
!         .. Array Arguments ..
          Complex (Kind=dp), Intent (In) :: a(lda, *), b(ldb, *)
          Complex (Kind=dp), Intent (Inout) :: c(ldc, *)
        End Subroutine
        Subroutine zherk(uplo, trans, n, k, alpha, a, lda, beta, c, ldc)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Scalar Arguments ..
          Real (Kind=dp), Intent (In) :: alpha, beta
          Integer, Intent (In) :: k, lda, ldc, n
          Character (1), Intent (In) :: trans, uplo
!         .. Array Arguments ..
          Complex (Kind=dp), Intent (In) :: a(lda, *)
          Complex (Kind=dp), Intent (Inout) :: c(ldc, *)
        End Subroutine
        Subroutine zhpmv(uplo, n, alpha, ap, x, incx, beta, y, incy)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Scalar Arguments ..
          Complex (Kind=dp), Intent (In) :: alpha, beta
          Integer, Intent (In) :: incx, incy, n
          Character (1), Intent (In) :: uplo
!         .. Array Arguments ..
          Complex (Kind=dp), Intent (In) :: ap(*), x(*)
          Complex (Kind=dp), Intent (Inout) :: y(*)
        End Subroutine
        Subroutine zhpr(uplo, n, alpha, x, incx, ap)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Scalar Arguments ..
          Real (Kind=dp), Intent (In) :: alpha
          Integer, Intent (In) :: incx, n
          Character (1), Intent (In) :: uplo
!         .. Array Arguments ..
          Complex (Kind=dp), Intent (Inout) :: ap(*)
          Complex (Kind=dp), Intent (In) :: x(*)
        End Subroutine
        Subroutine zhpr2(uplo, n, alpha, x, incx, y, incy, ap)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Scalar Arguments ..
          Complex (Kind=dp), Intent (In) :: alpha
          Integer, Intent (In) :: incx, incy, n
          Character (1), Intent (In) :: uplo
!         .. Array Arguments ..
          Complex (Kind=dp), Intent (Inout) :: ap(*)
          Complex (Kind=dp), Intent (In) :: x(*), y(*)
        End Subroutine
        Subroutine zscal(n, alpha, x, incx)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Scalar Arguments ..
          Complex (Kind=dp), Intent (In) :: alpha
          Integer, Intent (In) :: incx, n
!         .. Array Arguments ..
          Complex (Kind=dp), Intent (Inout) :: x(*)
        End Subroutine
        Subroutine zsctr(nz, x, indx, y)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Scalar Arguments ..
          Integer, Intent (In) :: nz
!         .. Array Arguments ..
          Complex (Kind=dp), Intent (In) :: x(*)
          Complex (Kind=dp), Intent (Inout) :: y(*)
          Integer, Intent (In) :: indx(*)
        End Subroutine
        Subroutine zswap(n, x, incx, y, incy)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Scalar Arguments ..
          Integer, Intent (In) :: incx, incy, n
!         .. Array Arguments ..
          Complex (Kind=dp), Intent (Inout) :: x(*), y(*)
        End Subroutine
        Subroutine zsymm(side, uplo, m, n, alpha, a, lda, b, ldb, beta, c, &
          ldc)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Scalar Arguments ..
          Complex (Kind=dp), Intent (In) :: alpha, beta
          Integer, Intent (In) :: lda, ldb, ldc, m, n
          Character (1), Intent (In) :: side, uplo
!         .. Array Arguments ..
          Complex (Kind=dp), Intent (In) :: a(lda, *), b(ldb, *)
          Complex (Kind=dp), Intent (Inout) :: c(ldc, *)
        End Subroutine
        Subroutine zsyr2k(uplo, trans, n, k, alpha, a, lda, b, ldb, beta, c, &
          ldc)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Scalar Arguments ..
          Complex (Kind=dp), Intent (In) :: alpha, beta
          Integer, Intent (In) :: k, lda, ldb, ldc, n
          Character (1), Intent (In) :: trans, uplo
!         .. Array Arguments ..
          Complex (Kind=dp), Intent (In) :: a(lda, *), b(ldb, *)
          Complex (Kind=dp), Intent (Inout) :: c(ldc, *)
        End Subroutine
        Subroutine zsyrk(uplo, trans, n, k, alpha, a, lda, beta, c, ldc)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Scalar Arguments ..
          Complex (Kind=dp), Intent (In) :: alpha, beta
          Integer, Intent (In) :: k, lda, ldc, n
          Character (1), Intent (In) :: trans, uplo
!         .. Array Arguments ..
          Complex (Kind=dp), Intent (In) :: a(lda, *)
          Complex (Kind=dp), Intent (Inout) :: c(ldc, *)
        End Subroutine
        Subroutine ztbmv(uplo, trans, diag, n, k, a, lda, x, incx)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Scalar Arguments ..
          Integer, Intent (In) :: incx, k, lda, n
          Character (1), Intent (In) :: diag, trans, uplo
!         .. Array Arguments ..
          Complex (Kind=dp), Intent (In) :: a(lda, *)
          Complex (Kind=dp), Intent (Inout) :: x(*)
        End Subroutine
        Subroutine ztbsv(uplo, trans, diag, n, k, a, lda, x, incx)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Scalar Arguments ..
          Integer, Intent (In) :: incx, k, lda, n
          Character (1), Intent (In) :: diag, trans, uplo
!         .. Array Arguments ..
          Complex (Kind=dp), Intent (In) :: a(lda, *)
          Complex (Kind=dp), Intent (Inout) :: x(*)
        End Subroutine
        Subroutine ztpmv(uplo, trans, diag, n, ap, x, incx)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Scalar Arguments ..
          Integer, Intent (In) :: incx, n
          Character (1), Intent (In) :: diag, trans, uplo
!         .. Array Arguments ..
          Complex (Kind=dp), Intent (In) :: ap(*)
          Complex (Kind=dp), Intent (Inout) :: x(*)
        End Subroutine
        Subroutine ztpsv(uplo, trans, diag, n, ap, x, incx)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Scalar Arguments ..
          Integer, Intent (In) :: incx, n
          Character (1), Intent (In) :: diag, trans, uplo
!         .. Array Arguments ..
          Complex (Kind=dp), Intent (In) :: ap(*)
          Complex (Kind=dp), Intent (Inout) :: x(*)
        End Subroutine
        Subroutine ztrmm(side, uplo, transa, diag, m, n, alpha, a, lda, b, &
          ldb)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Scalar Arguments ..
          Complex (Kind=dp), Intent (In) :: alpha
          Integer, Intent (In) :: lda, ldb, m, n
          Character (1), Intent (In) :: diag, side, transa, uplo
!         .. Array Arguments ..
          Complex (Kind=dp), Intent (In) :: a(lda, *)
          Complex (Kind=dp), Intent (Inout) :: b(ldb, *)
        End Subroutine
        Subroutine ztrmv(uplo, trans, diag, n, a, lda, x, incx)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Scalar Arguments ..
          Integer, Intent (In) :: incx, lda, n
          Character (1), Intent (In) :: diag, trans, uplo
!         .. Array Arguments ..
          Complex (Kind=dp), Intent (In) :: a(lda, *)
          Complex (Kind=dp), Intent (Inout) :: x(*)
        End Subroutine
        Subroutine ztrsm(side, uplo, transa, diag, m, n, alpha, a, lda, b, &
          ldb)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Scalar Arguments ..
          Complex (Kind=dp), Intent (In) :: alpha
          Integer, Intent (In) :: lda, ldb, m, n
          Character (1), Intent (In) :: diag, side, transa, uplo
!         .. Array Arguments ..
          Complex (Kind=dp), Intent (In) :: a(lda, *)
          Complex (Kind=dp), Intent (Inout) :: b(ldb, *)
        End Subroutine
        Subroutine ztrsv(uplo, trans, diag, n, a, lda, x, incx)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Scalar Arguments ..
          Integer, Intent (In) :: incx, lda, n
          Character (1), Intent (In) :: diag, trans, uplo
!         .. Array Arguments ..
          Complex (Kind=dp), Intent (In) :: a(lda, *)
          Complex (Kind=dp), Intent (Inout) :: x(*)
        End Subroutine
      End Interface
    End Module
