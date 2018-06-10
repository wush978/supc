/*
 * cblas_dnrm2.c
 *
 * The program is a C interface to dnrm2.
 * It calls the fortranwrapper before calling dnrm2.
 *
 * Written by Keita Teranishi.  2/11/1998
 * Modified by Wush Wu (2018)
 * 
*/

#include "cblas_R.h"

double cblas_Rdnrm2( const int N, const double *X, const int incX) 
{
#ifdef F77_INT
   F77_INT F77_N=N, F77_incX=incX;
#else 
   #define F77_N N
   #define F77_incX incX
#endif
   return F77_CALL(dnrm2)( &F77_N, X, &F77_incX);
}