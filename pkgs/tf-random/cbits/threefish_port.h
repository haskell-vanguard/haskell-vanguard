#ifndef _THREEFISH_PORT_H_
#define _THREEFISH_PORT_H_
/*******************************************************************
**
** Platform-specific definitions for Skein hash function.
**
** Source code author: Doug Whiting, 2008.
**
** This algorithm and source code is released to the public domain.
**
** Many thanks to Brian Gladman for his portable header files.
**
** To port Skein to an "unsupported" platform, change the definitions
** in this file appropriately.
** 
********************************************************************/

#include "brg_types.h"                      /* get integer type definitions */

typedef unsigned int    uint_t;             /* native unsigned integer */
typedef uint_8t         u08b_t;             /*  8-bit unsigned integer */
typedef uint_32t        u32b_t;             /* 32-bit unsigned integer */
typedef uint_64t        u64b_t;             /* 64-bit unsigned integer */

#ifndef RotL_64
#define RotL_64(x,N)    (((x) << (N)) | ((x) >> (64-(N))))
#endif

#endif
