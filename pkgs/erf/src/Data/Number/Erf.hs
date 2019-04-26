{-# LANGUAGE ForeignFunctionInterface #-}
module Data.Number.Erf(Erf(..), InvErf(..)) where
import Foreign.C

foreign import ccall "erf" c_erf :: CDouble -> CDouble
foreign import ccall "erfc" c_erfc :: CDouble -> CDouble
foreign import ccall "erff" c_erff :: CFloat -> CFloat
foreign import ccall "erfcf" c_erfcf :: CFloat -> CFloat

-- |Error function related functions.
--
-- The derivative of 'erf' is @\ x -> 2 / sqrt pi * exp (x^2)@,
-- and this uniquely determines 'erf' by @erf 0 = 0@.
--
-- Minimal complete definition is 'erfc' or 'normcdf'.
class (Floating a) => Erf a where
    erf :: a -> a
    erfc :: a -> a       -- ^@erfc x = 1 - erf x@
    erfcx :: a -> a      -- ^@erfcx x = exp (x*x) * erfc x@
    normcdf :: a -> a    -- ^@normcdf x = erfc(-x / sqrt 2) / 2@

    -- All the functions are inter-related, here's some defaults.
    erf x = 1 - erfc x
    erfc x = 2 * normcdf (-x * sqrt 2)
    erfcx x = exp (x*x) * erfc x
    normcdf x = erfc(-x / sqrt 2) / 2

instance Erf Double where
    erf = realToFrac . c_erf . realToFrac
    erfc = realToFrac . c_erfc . realToFrac

instance Erf Float where
    erf = realToFrac . c_erff . realToFrac
    erfc = realToFrac . c_erfcf . realToFrac

-- |Inverse error functions, e.g., @inverf . erf = id@ and @erf . inverf = id@ assuming
-- the appropriate codomain for 'inverf'.
-- Note that the accuracy may drop radically for extreme arguments.
class (Floating a) => InvErf a where
    inverf :: a -> a
    inverfc :: a -> a
--    inverfcx :: a -> a
    invnormcdf :: a -> a

    inverf p = inverfc (1 - p)
    inverfc p = - invnormcdf (p/2) / sqrt 2

instance InvErf Double where
    invnormcdf 0 = -1/0
    invnormcdf 1 = 1/0
    invnormcdf p =
        -- Do one iteration with Halley's root finder to get a more accurate result.
        let x = inorm p
            e = 0.5 * erfc (-x / sqrt 2) - p
            u = e * sqrt (2*pi) * exp (x*x / 2)
        in  x - u / (1 + x * u / 2)

instance InvErf Float where
    invnormcdf = inorm

-- Taken from http://home.online.no/~pjacklam/notes/invnorm/
-- Accurate to about 1e-9.
inorm :: (Ord a, Floating a) => a -> a
inorm p =
    let a1 = -3.969683028665376e+01
        a2 =  2.209460984245205e+02
        a3 = -2.759285104469687e+02
        a4 =  1.383577518672690e+02
        a5 = -3.066479806614716e+01
        a6 =  2.506628277459239e+00

        b1 = -5.447609879822406e+01
        b2 =  1.615858368580409e+02
        b3 = -1.556989798598866e+02
        b4 =  6.680131188771972e+01
        b5 = -1.328068155288572e+01

        c1 = -7.784894002430293e-03
        c2 = -3.223964580411365e-01
        c3 = -2.400758277161838e+00
        c4 = -2.549732539343734e+00
        c5 =  4.374664141464968e+00
        c6 =  2.938163982698783e+00

        d1 =  7.784695709041462e-03
        d2 =  3.224671290700398e-01
        d3 =  2.445134137142996e+00
        d4 =  3.754408661907416e+00

        pLow = 0.02425

        nan = 0/0

    in  if p < 0 then
            nan
        else if p == 0 then
            -1/0
        else if p < pLow then
            let q = sqrt(-2*log(p))
            in  (((((c1*q+c2)*q+c3)*q+c4)*q+c5)*q+c6) /
                 ((((d1*q+d2)*q+d3)*q+d4)*q+1)
        else if p < 1 - pLow then
            let q = p - 0.5
                r = q*q
            in  (((((a1*r+a2)*r+a3)*r+a4)*r+a5)*r+a6)*q /
                (((((b1*r+b2)*r+b3)*r+b4)*r+b5)*r+1)
        else if p <= 1 then
            - inorm (1 - p)
        else
            nan
