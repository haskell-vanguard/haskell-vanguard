{-# LANGUAGE NoMonomorphismRestriction #-}
-- | Library for control flow inside of monads with anaphoric variants on if and when and a C-like \"switch\" function.
-- 
-- Information: 
-- 
--   [@Author@] Jeff Heard
-- 
--   [@Copyright@] 2008 Jeff Heard
--   
--   [@License@] BSD
--  
--   [@Version@] 1.0
--
--   [@Status@] Alpha
module Control.Monad.IfElse where

import Control.Monad

-- A if with no else for unit returning thunks.  
--   Returns the value of the test.
-- when :: Monad m => Bool -> m () -> m Bool
-- when True action = action >> return True
-- when False _ = return False

-- | A if with no else for unit returning thunks.
--   Returns the value of the test.
whenM :: Monad m => m Bool -> m () -> m ()
whenM test action = test >>= \t -> if t then action else return ()

-- | Like a switch statement, and less cluttered than if else if
-- 
-- > cond [ (t1,a1), (t2,a2), ... ]
cond :: Monad m => [(Bool, m ())] -> m ()
cond [] = return ()
cond ((True,action) : _) = action 
cond ((False,_) : rest) = cond rest

-- | Like a switch statement, and less cluttered than if else if 
-- 
-- > condM [ (t1,a1), (t2,a2), ... ]
condM :: Monad m => [(m Bool, m ())] -> m ()
condM [] = return ()
condM ((test,action) : rest) = test >>= \t -> if t then action else condM rest

-- | Chainable anaphoric when.  Takes a maybe value.  
--  
-- if the value is Just x then execute @ action x @ , then return @ True @ .  otherwise return @ False @ .
awhen :: Monad m => Maybe a -> (a -> m ()) -> m ()
awhen Nothing _ = return ()
awhen (Just x) action = action x 

-- | Chainable anaphoric whenM.
awhenM :: Monad m => m (Maybe a) -> (a -> m ()) -> m ()
awhenM test action = test >>= \t -> case t of 
                                      Just x -> action x 
                                      Nothing -> return ()

-- | Anaphoric when-else chain.  Like a switch statement, but less cluttered
acond :: Monad m => [(Maybe a, a -> m ())] -> m ()
acond ((Nothing,_) : rest) = acond rest
acond ((Just x, action) : _) = action x 
acond [] = return ()

-- | Anaphoric if.
aif :: Monad m => Maybe a -> (a -> m b) -> m b -> m b
aif Nothing _ elseclause = elseclause
aif (Just x) ifclause _ = ifclause x

-- | Anaphoric if where the test is in Monad m.
aifM :: Monad m => m (Maybe a) -> (a -> m b) -> m b -> m b
aifM test ifclause elseclause = test >>= \t -> aif t ifclause elseclause

-- | Contrapositive of whenM, if not x then do y
unlessM a = whenM (liftM not $ a)

-- | unless-else chain.
ncond [] = return ()
ncond ((test , action) : rest) = if not test then action else ncond rest

-- | monadic unless-else chain
ncondM :: Monad m => [(m Bool, m ())] -> m ()
ncondM [] = return ()
ncondM ((test , action) : rest) = test >>= \t -> if not t then action else ncondM rest

-- | IO lifted @ && @
(&&^) = liftM2 (&&)

-- | IO lifted @ || @
(||^) = liftM2 (||)

-- | Conditionally do the right action based on the truth value of the left expression
(>>?) = when
infixl 1 >>?

-- | unless the left side is true, perform the right action
(>>!) = unless
infixl 1 >>!

-- | unless the (monadic) left side is true, perform the right action
(>>=>>!) = unlessM
infixl 1 >>=>>!

-- | Bind the result of the last expression in an anaphoric when.  
(>>=?) = awhen
infixl 1 >>=?

-- | composition of @ >>= @ and @ >>? @
(>>=>>?) = whenM
infixl 1 >>=>>?

-- | composition of @ >>= @ and @ >>=? @
(>>=>>=?) = awhenM
infixl 1 >>=>>=?

--
-- The following is from Control.Monad.Extras by Wren Thornton.
--

-- | Execute a monadic action so long as a monadic boolean returns
-- true.
{-# SPECIALIZE whileM :: IO Bool -> IO () -> IO () #-}
whileM                :: (Monad m) => m Bool -> m () -> m ()
whileM mb m = do b <- mb ; when b (m >> whileM mb m)


-- Named with M because 'Prelude.until' exists
-- | Negation of 'whileM': execute an action so long as the boolean
-- returns false.
{-# SPECIALIZE untilM :: IO Bool -> IO () -> IO () #-}
untilM                :: (Monad m) => m Bool -> m () -> m ()
untilM mb m = do b <- mb ; unless b (m >> untilM mb m)


-- | Strict version of 'return' because usually we don't need that
-- extra thunk.
{-# INLINE return' #-}
return'  :: (Monad m) => a -> m a
return' x = return $! x


-- | Take an action and make it into a side-effecting 'return'.
-- Because I seem to keep running into @m ()@ and the like.
infixr 8 `returning`
{-# INLINE returning #-}
returning      :: (Monad m) => (a -> m b) -> (a -> m a)
f `returning` x = f x >> return x


-- For reference this is also helpful:
-- >    liftM2 (>>) f g == \x -> f x >> g x


-- | This conversion is common enough to make a name for.
{-# INLINE maybeMP #-}
maybeMP :: (MonadPlus m) => Maybe a -> m a
maybeMP  = maybe mzero return

-- This rule should only fire when type-safe
{-# RULES "maybeMP/id" maybeMP = id #-}


