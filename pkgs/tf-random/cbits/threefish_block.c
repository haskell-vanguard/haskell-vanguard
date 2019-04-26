/***********************************************************************
**
** Implementation of the Threefish-256 block cipher. 
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
************************************************************************
**
** Implementation of the Skein block functions.
**
** Source code author: Doug Whiting, 2008.
**
** This algorithm and source code is released to the public domain.
**
************************************************************************/

#include <string.h>
#include "threefish.h"

#ifndef SKEIN_LOOP
#define SKEIN_LOOP 001                          /* default: unroll 256 and 512, but not 1024 */
#endif

#define BLK_BITS        (WCNT*64)               /* some useful definitions for code here */
#define KW_TWK_BASE     (0)
#define KW_KEY_BASE     (3)
#define ks              (kw + KW_KEY_BASE)                
#define ts              (kw + KW_TWK_BASE)

#ifdef SKEIN_DEBUG
#define DebugSaveTweak(ctx) { ctx->h.T[0] = ts[0]; ctx->h.T[1] = ts[1]; }
#else
#define DebugSaveTweak(ctx)
#endif

void    Threefish_256_Process_Block(const u08b_t *keyPtr, const u08b_t *blkPtr, u08b_t *cryptPtr, int w32out);

/* keyPtr, blkPtr and cryptPtr are all arrays of 4 64-bit unsingned ints in host-endian
 * format, unless the w32out argument is non-zero, in which case cryptPtr is an array of
 * 8 32-bit unsigned ints in host endian format. cryptPtr is the output array. The function
 * runs the cipher on one block only and ignores the tweak (tweak values are all 0). */
void    Threefish_256_Process_Block(const u08b_t *keyPtr, const u08b_t *blkPtr, u08b_t *cryptPtr, int w32out)
    { /* do it in C */
    enum
        {
        WCNT = SKEIN_256_STATE_WORDS
        };
#undef  RCNT
#define RCNT  (SKEIN_256_ROUNDS_TOTAL/8)

#ifdef  SKEIN_LOOP                              /* configure how much to unroll the loop */
#define SKEIN_UNROLL_256 (((SKEIN_LOOP)/100)%10)
#else
#define SKEIN_UNROLL_256 (0)
#endif

#if SKEIN_UNROLL_256
#if (RCNT % SKEIN_UNROLL_256)
#error "Invalid SKEIN_UNROLL_256"               /* sanity check on unroll count */
#endif
    size_t  r;
    u64b_t  kw[WCNT+4+RCNT*2];                  /* key schedule words : chaining vars + tweak + "rotation"*/
#else
    u64b_t  kw[WCNT+4];                         /* key schedule words : chaining vars + tweak */
#endif
    u64b_t  X0,X1,X2,X3;                        /* local copy of context vars, for speed */
    u64b_t  w [WCNT];                           /* local copy of input block */
#ifdef SKEIN_DEBUG
    const u64b_t *Xptr[4];                      /* use for debugging (help compiler put Xn in registers) */
    Xptr[0] = &X0;  Xptr[1] = &X1;  Xptr[2] = &X2;  Xptr[3] = &X3;
#endif
    /*Skein_assert(blkCnt != 0);*/                  /* never call with blkCnt == 0! */
    /* This is just adding the tweak
    ts[0] = ctx->h.T[0];
    ts[1] = ctx->h.T[1];*/

    /* Unnatural shift because of a removed loop */

        /* this implementation only supports 2**64 input bytes (no carry out here) */
        /*ts[0] += byteCntAdd; another tweak? */                   /* update processed length */

        /* precompute the key schedule for this block */
        /* get the key in little-endian format */
        /*Skein_Get64_LSB_First(ks,keyPtr,4);   */
        ks[0] = ((u64b_t *) keyPtr)[0];
        ks[1] = ((u64b_t *) (keyPtr + 8))[0];
        ks[2] = ((u64b_t *) (keyPtr + 16))[0];
        ks[3] = ((u64b_t *) (keyPtr + 24))[0];
        /*ks[0] = ctx->X[0];     
        ks[1] = ctx->X[1];
        ks[2] = ctx->X[2];
        ks[3] = ctx->X[3];*/
        ks[4] = ks[0] ^ ks[1] ^ ks[2] ^ ks[3] ^ SKEIN_KS_PARITY;

        /*ts[2] = ts[0] ^ ts[1]*/;
        ts[0] = 0;
        ts[1] = 0;
        ts[2] = 0;

        /*Skein_Get64_LSB_First(w,blkPtr,WCNT);*/   /* get input block in little-endian format */
        w[0] = ((u64b_t *) blkPtr)[0];
        w[1] = ((u64b_t *) (blkPtr + 8))[0];
        w[2] = ((u64b_t *) (blkPtr + 16))[0];
        w[3] = ((u64b_t *) (blkPtr + 24))[0];
		DebugSaveTweak(ctx);
        Skein_Show_Block(BLK_BITS,&ctx->h,ctx->X,blkPtr,w,ks,ts);

        X0 = w[0] + ks[0];                      /* do the first full key injection */
        X1 = w[1] + ks[1] + ts[0];
        X2 = w[2] + ks[2] + ts[1];
        X3 = w[3] + ks[3];

        Skein_Show_R_Ptr(BLK_BITS,&ctx->h,SKEIN_RND_KEY_INITIAL,Xptr);    /* show starting state values */

        /*blkPtr += SKEIN_256_BLOCK_BYTES;*/

        /* run the rounds */

#define Round256(p0,p1,p2,p3,ROT,rNum)                              \
    X##p0 += X##p1; X##p1 = RotL_64(X##p1,ROT##_0); X##p1 ^= X##p0; \
    X##p2 += X##p3; X##p3 = RotL_64(X##p3,ROT##_1); X##p3 ^= X##p2; \

#if SKEIN_UNROLL_256 == 0                       
#define R256(p0,p1,p2,p3,ROT,rNum)           /* fully unrolled */   \
    Round256(p0,p1,p2,p3,ROT,rNum)                                  \
    Skein_Show_R_Ptr(BLK_BITS,&ctx->h,rNum,Xptr);

#define I256(R)                                                     \
    X0   += ks[((R)+1) % 5];    /* inject the key schedule value */ \
    X1   += ks[((R)+2) % 5] + ts[((R)+1) % 3];                      \
    X2   += ks[((R)+3) % 5] + ts[((R)+2) % 3];                      \
    X3   += ks[((R)+4) % 5] +     (R)+1;                            \
    Skein_Show_R_Ptr(BLK_BITS,&ctx->h,SKEIN_RND_KEY_INJECT,Xptr);
#else                                       /* looping version */
#define R256(p0,p1,p2,p3,ROT,rNum)                                  \
    Round256(p0,p1,p2,p3,ROT,rNum)                                  \
    Skein_Show_R_Ptr(BLK_BITS,&ctx->h,4*(r-1)+rNum,Xptr);

#define I256(R)                                                     \
    X0   += ks[r+(R)+0];        /* inject the key schedule value */ \
    X1   += ks[r+(R)+1] + ts[r+(R)+0];                              \
    X2   += ks[r+(R)+2] + ts[r+(R)+1];                              \
    X3   += ks[r+(R)+3] +    r+(R)   ;                              \
    ks[r + (R)+4    ]   = ks[r+(R)-1];     /* rotate key schedule */\
    ts[r + (R)+2    ]   = ts[r+(R)-1];                              \
    Skein_Show_R_Ptr(BLK_BITS,&ctx->h,SKEIN_RND_KEY_INJECT,Xptr);

    for (r=1;r < 2*RCNT;r+=2*SKEIN_UNROLL_256)  /* loop thru it */
#endif  
        {    
#define R256_8_rounds(R)                  \
        R256(0,1,2,3,R_256_0,8*(R) + 1);  \
        R256(0,3,2,1,R_256_1,8*(R) + 2);  \
        R256(0,1,2,3,R_256_2,8*(R) + 3);  \
        R256(0,3,2,1,R_256_3,8*(R) + 4);  \
        I256(2*(R));                      \
        R256(0,1,2,3,R_256_4,8*(R) + 5);  \
        R256(0,3,2,1,R_256_5,8*(R) + 6);  \
        R256(0,1,2,3,R_256_6,8*(R) + 7);  \
        R256(0,3,2,1,R_256_7,8*(R) + 8);  \
        I256(2*(R)+1);

        R256_8_rounds( 0);

#define R256_Unroll_R(NN) ((SKEIN_UNROLL_256 == 0 && SKEIN_256_ROUNDS_TOTAL/8 > (NN)) || (SKEIN_UNROLL_256 > (NN)))

  #if   R256_Unroll_R( 1)
        R256_8_rounds( 1);
  #endif
  #if   R256_Unroll_R( 2)
        R256_8_rounds( 2);
  #endif
  #if   R256_Unroll_R( 3)
        R256_8_rounds( 3);
  #endif
  #if   R256_Unroll_R( 4)
        R256_8_rounds( 4);
  #endif
  #if   R256_Unroll_R( 5)
        R256_8_rounds( 5);
  #endif
  #if   R256_Unroll_R( 6)
        R256_8_rounds( 6);
  #endif
  #if   R256_Unroll_R( 7)
        R256_8_rounds( 7);
  #endif
  #if   R256_Unroll_R( 8)
        R256_8_rounds( 8);
  #endif
  #if   R256_Unroll_R( 9)
        R256_8_rounds( 9);
  #endif
  #if   R256_Unroll_R(10)
        R256_8_rounds(10);
  #endif
  #if   R256_Unroll_R(11)
        R256_8_rounds(11);
  #endif
  #if   R256_Unroll_R(12)
        R256_8_rounds(12);
  #endif
  #if   R256_Unroll_R(13)
        R256_8_rounds(13);
  #endif
  #if   R256_Unroll_R(14)
        R256_8_rounds(14);
  #endif
  #if  (SKEIN_UNROLL_256 > 14)
#error  "need more unrolling in Skein_256_Process_Block"
  #endif
        }
        /* do the final "feedforward" xor, update context chaining vars */
        /*ctx->X[0] = X0 ^ w[0];
        ctx->X[1] = X1 ^ w[1];
        ctx->X[2] = X2 ^ w[2];
        ctx->X[3] = X3 ^ w[3];*/
        if (w32out) {
            ((u32b_t *) cryptPtr)       [0] = X0 >> 32;
            ((u32b_t *) (cryptPtr + 4)) [0] = X0;
            ((u32b_t *) (cryptPtr + 8)) [0] = X1 >> 32;
            ((u32b_t *) (cryptPtr + 12))[0] = X1;
            ((u32b_t *) (cryptPtr + 16))[0] = X2 >> 32;
            ((u32b_t *) (cryptPtr + 20))[0] = X2;
            ((u32b_t *) (cryptPtr + 24))[0] = X3 >> 32;
            ((u32b_t *) (cryptPtr + 28))[0] = X3;
        } else {
            ((u64b_t *) cryptPtr)       [0] = X0;
            ((u64b_t *) (cryptPtr + 8)) [0] = X1;
            ((u64b_t *) (cryptPtr + 16))[0] = X2;
            ((u64b_t *) (cryptPtr + 24))[0] = X3;
        }
        Skein_Show_Round(BLK_BITS,&ctx->h,SKEIN_RND_FEED_FWD,ctx->X);

        /*ts[1] &= ~SKEIN_T1_FLAG_FIRST;*/
    }

#if defined(SKEIN_CODE_SIZE) || defined(SKEIN_PERF)
size_t Threefish_256_Process_Block_CodeSize(void)
    {
    return ((u08b_t *) Threefish_256_Process_Block_CodeSize) -
           ((u08b_t *) Threefish_256_Process_Block_Block);
    }
uint_t Threefish_256_Unroll_Cnt(void)
    {
    return SKEIN_UNROLL_256;
    }
#endif

