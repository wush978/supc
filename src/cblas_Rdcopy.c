/*
 * cblas_dcopy.c
 *
 * The program is a C interface to dcopy.
 *
 * Written by Keita Teranishi.  2/11/1998
 *
 */
#include "cblas_R.h"
void cblas_Rdcopy( const int N, const double *X,
                      const int incX, double *Y, const int incY)
{
#ifdef F77_INT
   F77_INT F77_N=N, F77_incX=incX, F77_incY=incY;
#else 
   #define F77_N N
   #define F77_incX incX
   #define F77_incY incY
#endif
   F77_CALL(dcopy)( &F77_N, X, &F77_incX, Y, &F77_incY);
}
