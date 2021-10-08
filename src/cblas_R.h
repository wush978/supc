#ifndef __CBLAS_DGEMM_H__
#define __CBLAS_DGEMM_H__

#define USE_FC_LEN_T

#include <R_ext/BLAS.h>

enum CBLAS_ORDER {CblasRowMajor=101, CblasColMajor=102};
enum CBLAS_TRANSPOSE {CblasNoTrans=111, CblasTrans=112, CblasConjTrans=113};
enum CBLAS_UPLO {CblasUpper=121, CblasLower=122};
enum CBLAS_DIAG {CblasNonUnit=131, CblasUnit=132};
enum CBLAS_SIDE {CblasLeft=141, CblasRight=142};

#ifdef __cplusplus
extern "C" {
#endif

int cblas_Rdgemm(const enum CBLAS_ORDER Order, const enum CBLAS_TRANSPOSE TransA,
                 const enum CBLAS_TRANSPOSE TransB, const int M, const int N,
                 const int K, const double alpha, const double *A,
                 const int lda, const double *B, const int ldb,
                 const double beta, double *C, const int ldc);

void cblas_Rdcopy( const int N, const double *X,
                      const int incX, double *Y, const int incY);

void cblas_Rdaxpy( const int N, const double alpha, const double *X,
                     const int incX, double *Y, const int incY);

double cblas_Rdnrm2(const int N, const double *X, const int incX);

#ifdef __cplusplus
}
#endif

#endif // __CBLAS_DGEMM_H__