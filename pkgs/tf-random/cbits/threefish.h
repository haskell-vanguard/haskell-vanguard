#ifndef _THREEFISH_H_
#define _THREEFISH_H_     1


/***********************************************************************
**
** Interface declarations and internal definitions for the Threefish-256
** cipher.
**
** Copyright (c) 2012, Michał Pałka
** All rights reserved
**
** Redistribution and use in source and binary forms, with or without
** modification, are permitted provided that the following conditions are met:
**     * Redistributions of source code must retain the above copyright
**       notice, this list of conditions and the following disclaimer.
**     * Redistributions in binary form must reproduce the above copyright
**       notice, this list of conditions and the following disclaimer in the
**       documentation and/or other materials provided with the distribution.
**     * The names of the authors may not be used to endorse or promote
**       products derived from this software without specific prior written
**       permission.
** 
** THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
** ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
** WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
** DISCLAIMED. IN NO EVENT SHALL <COPYRIGHT HOLDER> BE LIABLE FOR ANY
** DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
** (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
** LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
** ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
** (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
** SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
** 
**
** This code is extracted, with some simplifications, from the Skein
** team submission for the NIST SHA-3 competition. Original authorship is
** stated below.
**
**
***************************************************************************
**
** Interface declarations and internal definitions for Skein hashing.
**
** Source code author: Doug Whiting, 2008.
**
** This algorithm and source code is released to the public domain.
**
***************************************************************************
** 
** The following compile-time switches may be defined to control some
** tradeoffs between speed, code size, error checking, and security.
**
** The "default" note explains what happens when the switch is not defined.
**
**  SKEIN_DEBUG            -- make callouts from inside Skein code
**                            to examine/display intermediate values.
**                            [default: no callouts (no overhead)]
**
**  SKEIN_ERR_CHECK        -- how error checking is handled inside Skein
**                            code. If not defined, most error checking 
**                            is disabled (for performance). Otherwise, 
**                            the switch value is interpreted as:
**                                0: use assert()      to flag errors
**                                1: return SKEIN_FAIL to flag errors
**
***************************************************************************/
#ifdef __cplusplus
extern "C"
{
#endif

#include <stddef.h>                          /* get size_t definition */
#include "threefish_port.h"                      /* get platform-specific definitions */


#define  SKEIN_MODIFIER_WORDS  ( 2)          /* number of modifier (tweak) words */

#define  SKEIN_256_STATE_WORDS ( 4)

#define  SKEIN_256_STATE_BYTES ( 8*SKEIN_256_STATE_WORDS)

#define  SKEIN_256_STATE_BITS  (64*SKEIN_256_STATE_WORDS)

#define  SKEIN_256_BLOCK_BYTES ( 8*SKEIN_256_STATE_WORDS)


#define SKEIN_MK_64(hi32,lo32)  ((lo32) + (((u64b_t) (hi32)) << 32))
#define SKEIN_KS_PARITY         SKEIN_MK_64(0x1BD11BDA,0xA9FC1A22)

/*****************************************************************
** "Internal" Skein definitions for debugging and error checking
******************************************************************/
#ifdef  SKEIN_DEBUG             /* examine/display intermediate values? */
#include "skein_debug.h"
#else                           /* default is no callouts */
#define Skein_Show_Block(bits,ctx,X,blkPtr,wPtr,ksEvenPtr,ksOddPtr)
#define Skein_Show_Round(bits,ctx,r,X)
#define Skein_Show_R_Ptr(bits,ctx,r,X_ptr)
#define Skein_Show_Final(bits,ctx,cnt,outPtr)
#define Skein_Show_Key(bits,ctx,key,keyBytes)
#endif

#ifndef SKEIN_ERR_CHECK        /* run-time checks (e.g., bad params, uninitialized context)? */
#define Skein_Assert(x,retCode)/* default: ignore all Asserts, for performance */
#define Skein_assert(x)
#elif   defined(SKEIN_ASSERT)
#include <assert.h>     
#define Skein_Assert(x,retCode) assert(x) 
#define Skein_assert(x)         assert(x) 
#else
#include <assert.h>     
#define Skein_Assert(x,retCode) { if (!(x)) return retCode; } /*  caller  error */
#define Skein_assert(x)         assert(x)                     /* internal error */
#endif

enum    
    {   
        /* Threefish_256 round rotation constants */
    R_256_0_0=14, R_256_0_1=16,
    R_256_1_0=52, R_256_1_1=57,
    R_256_2_0=23, R_256_2_1=40,
    R_256_3_0= 5, R_256_3_1=37,
    R_256_4_0=25, R_256_4_1=33,
    R_256_5_0=46, R_256_5_1=12,
    R_256_6_0=58, R_256_6_1=22,
    R_256_7_0=32, R_256_7_1=32,

    };

#ifndef SKEIN_ROUNDS
#define SKEIN_256_ROUNDS_TOTAL (72)          /* number of rounds for the different block sizes */
#else                                        /* allow command-line define in range 8*(5..14)   */
#define SKEIN_256_ROUNDS_TOTAL (8*((((SKEIN_ROUNDS/100) + 5) % 10) + 5))
#endif

#ifdef __cplusplus
}
#endif

#endif  /* ifndef _SKEIN_H_ */
