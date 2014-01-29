{-# OPTIONS_GHC -w #-}
module Parser where

import Lexer
import Syntax

-- parser produced by Happy Version 1.18.10

data HappyAbsSyn 
	= HappyTerminal (Token)
	| HappyErrorToken Int
	| HappyAbsSyn4 (Program)
	| HappyAbsSyn5 (Expression)
	| HappyAbsSyn6 ([Command])
	| HappyAbsSyn7 (Command)

{- to allow type-synonyms as our monads (likely
 - with explicitly-specified bind and return)
 - in Haskell98, it seems that with
 - /type M a = .../, then /(HappyReduction M)/
 - is not allowed.  But Happy is a
 - code-generator that can just substitute it.
type HappyReduction m = 
	   Int 
	-> (Token)
	-> HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> m HappyAbsSyn)
	-> [HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> m HappyAbsSyn)] 
	-> HappyStk HappyAbsSyn 
	-> [(Token)] -> m HappyAbsSyn
-}

action_0,
 action_1,
 action_2,
 action_3,
 action_4,
 action_5,
 action_6,
 action_7,
 action_8,
 action_9,
 action_10,
 action_11,
 action_12,
 action_13,
 action_14,
 action_15,
 action_16,
 action_17,
 action_18,
 action_19,
 action_20,
 action_21,
 action_22,
 action_23,
 action_24,
 action_25,
 action_26,
 action_27,
 action_28,
 action_29,
 action_30,
 action_31,
 action_32,
 action_33,
 action_34,
 action_35,
 action_36,
 action_37 :: () => Int -> ({-HappyReduction (HappyIdentity) = -}
	   Int 
	-> (Token)
	-> HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> (HappyIdentity) HappyAbsSyn)
	-> [HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> (HappyIdentity) HappyAbsSyn)] 
	-> HappyStk HappyAbsSyn 
	-> [(Token)] -> (HappyIdentity) HappyAbsSyn)

happyReduce_1,
 happyReduce_2,
 happyReduce_3,
 happyReduce_4,
 happyReduce_5,
 happyReduce_6,
 happyReduce_7,
 happyReduce_8,
 happyReduce_9,
 happyReduce_10,
 happyReduce_11,
 happyReduce_12,
 happyReduce_13 :: () => ({-HappyReduction (HappyIdentity) = -}
	   Int 
	-> (Token)
	-> HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> (HappyIdentity) HappyAbsSyn)
	-> [HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> (HappyIdentity) HappyAbsSyn)] 
	-> HappyStk HappyAbsSyn 
	-> [(Token)] -> (HappyIdentity) HappyAbsSyn)

action_0 (22) = happyShift action_2
action_0 (4) = happyGoto action_3
action_0 _ = happyFail

action_1 (22) = happyShift action_2
action_1 _ = happyFail

action_2 (24) = happyShift action_4
action_2 _ = happyFail

action_3 (25) = happyAccept
action_3 _ = happyFail

action_4 (16) = happyShift action_5
action_4 _ = happyFail

action_5 (20) = happyShift action_8
action_5 (24) = happyShift action_9
action_5 (6) = happyGoto action_6
action_5 (7) = happyGoto action_7
action_5 _ = happyReduce_11

action_6 (23) = happyShift action_20
action_6 _ = happyFail

action_7 (16) = happyShift action_19
action_7 _ = happyFail

action_8 (9) = happyShift action_12
action_8 (13) = happyShift action_13
action_8 (15) = happyShift action_14
action_8 (17) = happyShift action_15
action_8 (18) = happyShift action_16
action_8 (19) = happyShift action_17
action_8 (24) = happyShift action_18
action_8 (5) = happyGoto action_11
action_8 _ = happyFail

action_9 (14) = happyShift action_10
action_9 _ = happyFail

action_10 (9) = happyShift action_12
action_10 (13) = happyShift action_13
action_10 (15) = happyShift action_14
action_10 (17) = happyShift action_15
action_10 (18) = happyShift action_16
action_10 (19) = happyShift action_17
action_10 (24) = happyShift action_18
action_10 (5) = happyGoto action_30
action_10 _ = happyFail

action_11 (8) = happyShift action_28
action_11 (21) = happyShift action_29
action_11 _ = happyFail

action_12 (9) = happyShift action_12
action_12 (13) = happyShift action_13
action_12 (15) = happyShift action_14
action_12 (17) = happyShift action_15
action_12 (18) = happyShift action_16
action_12 (19) = happyShift action_17
action_12 (24) = happyShift action_18
action_12 (5) = happyGoto action_27
action_12 _ = happyFail

action_13 (9) = happyShift action_12
action_13 (13) = happyShift action_13
action_13 (15) = happyShift action_14
action_13 (17) = happyShift action_15
action_13 (18) = happyShift action_16
action_13 (19) = happyShift action_17
action_13 (24) = happyShift action_18
action_13 (5) = happyGoto action_26
action_13 _ = happyFail

action_14 _ = happyReduce_4

action_15 (9) = happyShift action_12
action_15 (13) = happyShift action_13
action_15 (15) = happyShift action_14
action_15 (17) = happyShift action_15
action_15 (18) = happyShift action_16
action_15 (19) = happyShift action_17
action_15 (24) = happyShift action_18
action_15 (5) = happyGoto action_25
action_15 _ = happyFail

action_16 (9) = happyShift action_12
action_16 (13) = happyShift action_13
action_16 (15) = happyShift action_14
action_16 (17) = happyShift action_15
action_16 (18) = happyShift action_16
action_16 (19) = happyShift action_17
action_16 (24) = happyShift action_18
action_16 (5) = happyGoto action_24
action_16 _ = happyFail

action_17 (9) = happyShift action_12
action_17 (13) = happyShift action_13
action_17 (15) = happyShift action_14
action_17 (17) = happyShift action_15
action_17 (18) = happyShift action_16
action_17 (19) = happyShift action_17
action_17 (24) = happyShift action_18
action_17 (5) = happyGoto action_23
action_17 _ = happyFail

action_18 _ = happyReduce_5

action_19 (20) = happyShift action_8
action_19 (24) = happyShift action_9
action_19 (6) = happyGoto action_22
action_19 (7) = happyGoto action_7
action_19 _ = happyReduce_11

action_20 (9) = happyShift action_12
action_20 (13) = happyShift action_13
action_20 (15) = happyShift action_14
action_20 (17) = happyShift action_15
action_20 (18) = happyShift action_16
action_20 (19) = happyShift action_17
action_20 (24) = happyShift action_18
action_20 (5) = happyGoto action_21
action_20 _ = happyFail

action_21 (8) = happyShift action_28
action_21 _ = happyReduce_1

action_22 _ = happyReduce_10

action_23 (8) = happyShift action_28
action_23 _ = happyReduce_8

action_24 (8) = happyShift action_28
action_24 _ = happyReduce_7

action_25 (8) = happyShift action_28
action_25 (9) = happyShift action_12
action_25 (13) = happyShift action_13
action_25 (15) = happyShift action_14
action_25 (17) = happyShift action_15
action_25 (18) = happyShift action_16
action_25 (19) = happyShift action_17
action_25 (24) = happyShift action_18
action_25 (5) = happyGoto action_35
action_25 _ = happyFail

action_26 (8) = happyShift action_28
action_26 (9) = happyShift action_12
action_26 (13) = happyShift action_13
action_26 (15) = happyShift action_14
action_26 (17) = happyShift action_15
action_26 (18) = happyShift action_16
action_26 (19) = happyShift action_17
action_26 (24) = happyShift action_18
action_26 (5) = happyGoto action_34
action_26 _ = happyFail

action_27 (8) = happyShift action_28
action_27 (10) = happyShift action_33
action_27 _ = happyFail

action_28 (9) = happyShift action_12
action_28 (13) = happyShift action_13
action_28 (15) = happyShift action_14
action_28 (17) = happyShift action_15
action_28 (18) = happyShift action_16
action_28 (19) = happyShift action_17
action_28 (24) = happyShift action_18
action_28 (5) = happyGoto action_32
action_28 _ = happyFail

action_29 (11) = happyShift action_31
action_29 _ = happyFail

action_30 (8) = happyShift action_28
action_30 _ = happyReduce_13

action_31 (20) = happyShift action_8
action_31 (24) = happyShift action_9
action_31 (6) = happyGoto action_36
action_31 (7) = happyGoto action_7
action_31 _ = happyReduce_11

action_32 (8) = happyShift action_28
action_32 _ = happyReduce_3

action_33 _ = happyReduce_6

action_34 (8) = happyShift action_28
action_34 _ = happyReduce_9

action_35 (8) = happyShift action_28
action_35 _ = happyReduce_2

action_36 (12) = happyShift action_37
action_36 _ = happyFail

action_37 _ = happyReduce_12

happyReduce_1 = happyReduce 6 4 happyReduction_1
happyReduction_1 ((HappyAbsSyn5  happy_var_6) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn6  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokenVar happy_var_2)) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn4
		 (Program happy_var_2 happy_var_4 happy_var_6
	) `HappyStk` happyRest

happyReduce_2 = happySpecReduce_3  5 happyReduction_2
happyReduction_2 (HappyAbsSyn5  happy_var_3)
	(HappyAbsSyn5  happy_var_2)
	_
	 =  HappyAbsSyn5
		 (Cons happy_var_2 happy_var_3
	)
happyReduction_2 _ _ _  = notHappyAtAll 

happyReduce_3 = happySpecReduce_3  5 happyReduction_3
happyReduction_3 (HappyAbsSyn5  happy_var_3)
	_
	(HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn5
		 (Cons happy_var_1 happy_var_3
	)
happyReduction_3 _ _ _  = notHappyAtAll 

happyReduce_4 = happySpecReduce_1  5 happyReduction_4
happyReduction_4 _
	 =  HappyAbsSyn5
		 (Nil
	)

happyReduce_5 = happySpecReduce_1  5 happyReduction_5
happyReduction_5 (HappyTerminal (TokenVar happy_var_1))
	 =  HappyAbsSyn5
		 (Var happy_var_1
	)
happyReduction_5 _  = notHappyAtAll 

happyReduce_6 = happySpecReduce_3  5 happyReduction_6
happyReduction_6 _
	(HappyAbsSyn5  happy_var_2)
	_
	 =  HappyAbsSyn5
		 (happy_var_2
	)
happyReduction_6 _ _ _  = notHappyAtAll 

happyReduce_7 = happySpecReduce_2  5 happyReduction_7
happyReduction_7 (HappyAbsSyn5  happy_var_2)
	_
	 =  HappyAbsSyn5
		 (Hd happy_var_2
	)
happyReduction_7 _ _  = notHappyAtAll 

happyReduce_8 = happySpecReduce_2  5 happyReduction_8
happyReduction_8 (HappyAbsSyn5  happy_var_2)
	_
	 =  HappyAbsSyn5
		 (Tl happy_var_2
	)
happyReduction_8 _ _  = notHappyAtAll 

happyReduce_9 = happySpecReduce_3  5 happyReduction_9
happyReduction_9 (HappyAbsSyn5  happy_var_3)
	(HappyAbsSyn5  happy_var_2)
	_
	 =  HappyAbsSyn5
		 (IsEq happy_var_2 happy_var_3
	)
happyReduction_9 _ _ _  = notHappyAtAll 

happyReduce_10 = happySpecReduce_3  6 happyReduction_10
happyReduction_10 (HappyAbsSyn6  happy_var_3)
	_
	(HappyAbsSyn7  happy_var_1)
	 =  HappyAbsSyn6
		 (happy_var_1 : happy_var_3
	)
happyReduction_10 _ _ _  = notHappyAtAll 

happyReduce_11 = happySpecReduce_0  6 happyReduction_11
happyReduction_11  =  HappyAbsSyn6
		 ([]
	)

happyReduce_12 = happyReduce 6 7 happyReduction_12
happyReduction_12 (_ `HappyStk`
	(HappyAbsSyn6  happy_var_5) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn5  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn7
		 (While happy_var_2 happy_var_5
	) `HappyStk` happyRest

happyReduce_13 = happySpecReduce_3  7 happyReduction_13
happyReduction_13 (HappyAbsSyn5  happy_var_3)
	_
	(HappyTerminal (TokenVar happy_var_1))
	 =  HappyAbsSyn7
		 (Assign happy_var_1 happy_var_3
	)
happyReduction_13 _ _ _  = notHappyAtAll 

happyNewToken action sts stk [] =
	action 25 25 notHappyAtAll (HappyState action) sts stk []

happyNewToken action sts stk (tk:tks) =
	let cont i = action i i tk (HappyState action) sts stk tks in
	case tk of {
	TokenConsInf -> cont 8;
	TokenOpenBrc -> cont 9;
	TokenClosBrc -> cont 10;
	TokenOpenCur -> cont 11;
	TokenClosCur -> cont 12;
	TokenIsEq -> cont 13;
	TokenAssign -> cont 14;
	TokenNil -> cont 15;
	TokenSemiCo -> cont 16;
	TokenConsPos -> cont 17;
	TokenHead -> cont 18;
	TokenTail -> cont 19;
	TokenWhile -> cont 20;
	TokenDo -> cont 21;
	TokenRead -> cont 22;
	TokenWrite -> cont 23;
	TokenVar happy_dollar_dollar -> cont 24;
	_ -> happyError' (tk:tks)
	}

happyError_ 25 tk tks = happyError' tks
happyError_ _ tk tks = happyError' (tk:tks)

newtype HappyIdentity a = HappyIdentity a
happyIdentity = HappyIdentity
happyRunIdentity (HappyIdentity a) = a

instance Monad HappyIdentity where
    return = HappyIdentity
    (HappyIdentity p) >>= q = q p

happyThen :: () => HappyIdentity a -> (a -> HappyIdentity b) -> HappyIdentity b
happyThen = (>>=)
happyReturn :: () => a -> HappyIdentity a
happyReturn = (return)
happyThen1 m k tks = (>>=) m (\a -> k a tks)
happyReturn1 :: () => a -> b -> HappyIdentity a
happyReturn1 = \a tks -> (return) a
happyError' :: () => [(Token)] -> HappyIdentity a
happyError' = HappyIdentity . parseError

parse tks = happyRunIdentity happySomeParser where
  happySomeParser = happyThen (happyParse action_0 tks) (\x -> case x of {HappyAbsSyn4 z -> happyReturn z; _other -> notHappyAtAll })

happySeq = happyDontSeq


parseError :: [Token] -> a
parseError _ = error "Parse error"
{-# LINE 1 "templates/GenericTemplate.hs" #-}
{-# LINE 1 "templates/GenericTemplate.hs" #-}
{-# LINE 1 "<command-line>" #-}





# 1 "/usr/include/stdc-predef.h" 1 3 4

# 17 "/usr/include/stdc-predef.h" 3 4























# 5 "<command-line>" 2
{-# LINE 1 "templates/GenericTemplate.hs" #-}
-- Id: GenericTemplate.hs,v 1.26 2005/01/14 14:47:22 simonmar Exp 

{-# LINE 30 "templates/GenericTemplate.hs" #-}








{-# LINE 51 "templates/GenericTemplate.hs" #-}

{-# LINE 61 "templates/GenericTemplate.hs" #-}

{-# LINE 70 "templates/GenericTemplate.hs" #-}

infixr 9 `HappyStk`
data HappyStk a = HappyStk a (HappyStk a)

-----------------------------------------------------------------------------
-- starting the parse

happyParse start_state = happyNewToken start_state notHappyAtAll notHappyAtAll

-----------------------------------------------------------------------------
-- Accepting the parse

-- If the current token is (1), it means we've just accepted a partial
-- parse (a %partial parser).  We must ignore the saved token on the top of
-- the stack in this case.
happyAccept (1) tk st sts (_ `HappyStk` ans `HappyStk` _) =
	happyReturn1 ans
happyAccept j tk st sts (HappyStk ans _) = 
	 (happyReturn1 ans)

-----------------------------------------------------------------------------
-- Arrays only: do the next action

{-# LINE 148 "templates/GenericTemplate.hs" #-}

-----------------------------------------------------------------------------
-- HappyState data type (not arrays)



newtype HappyState b c = HappyState
        (Int ->                    -- token number
         Int ->                    -- token number (yes, again)
         b ->                           -- token semantic value
         HappyState b c ->              -- current state
         [HappyState b c] ->            -- state stack
         c)



-----------------------------------------------------------------------------
-- Shifting a token

happyShift new_state (1) tk st sts stk@(x `HappyStk` _) =
     let (i) = (case x of { HappyErrorToken (i) -> i }) in
--     trace "shifting the error token" $
     new_state i i tk (HappyState (new_state)) ((st):(sts)) (stk)

happyShift new_state i tk st sts stk =
     happyNewToken new_state ((st):(sts)) ((HappyTerminal (tk))`HappyStk`stk)

-- happyReduce is specialised for the common cases.

happySpecReduce_0 i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happySpecReduce_0 nt fn j tk st@((HappyState (action))) sts stk
     = action nt j tk st ((st):(sts)) (fn `HappyStk` stk)

happySpecReduce_1 i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happySpecReduce_1 nt fn j tk _ sts@(((st@(HappyState (action))):(_))) (v1`HappyStk`stk')
     = let r = fn v1 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_2 i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happySpecReduce_2 nt fn j tk _ ((_):(sts@(((st@(HappyState (action))):(_))))) (v1`HappyStk`v2`HappyStk`stk')
     = let r = fn v1 v2 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_3 i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happySpecReduce_3 nt fn j tk _ ((_):(((_):(sts@(((st@(HappyState (action))):(_))))))) (v1`HappyStk`v2`HappyStk`v3`HappyStk`stk')
     = let r = fn v1 v2 v3 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happyReduce k i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happyReduce k nt fn j tk st sts stk
     = case happyDrop (k - ((1) :: Int)) sts of
	 sts1@(((st1@(HappyState (action))):(_))) ->
        	let r = fn stk in  -- it doesn't hurt to always seq here...
       		happyDoSeq r (action nt j tk st1 sts1 r)

happyMonadReduce k nt fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happyMonadReduce k nt fn j tk st sts stk =
        happyThen1 (fn stk tk) (\r -> action nt j tk st1 sts1 (r `HappyStk` drop_stk))
       where (sts1@(((st1@(HappyState (action))):(_)))) = happyDrop k ((st):(sts))
             drop_stk = happyDropStk k stk

happyMonad2Reduce k nt fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happyMonad2Reduce k nt fn j tk st sts stk =
       happyThen1 (fn stk tk) (\r -> happyNewToken new_state sts1 (r `HappyStk` drop_stk))
       where (sts1@(((st1@(HappyState (action))):(_)))) = happyDrop k ((st):(sts))
             drop_stk = happyDropStk k stk





             new_state = action


happyDrop (0) l = l
happyDrop n ((_):(t)) = happyDrop (n - ((1) :: Int)) t

happyDropStk (0) l = l
happyDropStk n (x `HappyStk` xs) = happyDropStk (n - ((1)::Int)) xs

-----------------------------------------------------------------------------
-- Moving to a new state after a reduction

{-# LINE 246 "templates/GenericTemplate.hs" #-}
happyGoto action j tk st = action j j tk (HappyState action)


-----------------------------------------------------------------------------
-- Error recovery ((1) is the error token)

-- parse error if we are in recovery and we fail again
happyFail (1) tk old_st _ stk@(x `HappyStk` _) =
     let (i) = (case x of { HappyErrorToken (i) -> i }) in
--	trace "failing" $ 
        happyError_ i tk

{-  We don't need state discarding for our restricted implementation of
    "error".  In fact, it can cause some bogus parses, so I've disabled it
    for now --SDM

-- discard a state
happyFail  (1) tk old_st (((HappyState (action))):(sts)) 
						(saved_tok `HappyStk` _ `HappyStk` stk) =
--	trace ("discarding state, depth " ++ show (length stk))  $
	action (1) (1) tk (HappyState (action)) sts ((saved_tok`HappyStk`stk))
-}

-- Enter error recovery: generate an error token,
--                       save the old token and carry on.
happyFail  i tk (HappyState (action)) sts stk =
--      trace "entering error recovery" $
	action (1) (1) tk (HappyState (action)) sts ( (HappyErrorToken (i)) `HappyStk` stk)

-- Internal happy errors:

notHappyAtAll :: a
notHappyAtAll = error "Internal Happy error\n"

-----------------------------------------------------------------------------
-- Hack to get the typechecker to accept our action functions







-----------------------------------------------------------------------------
-- Seq-ing.  If the --strict flag is given, then Happy emits 
--	happySeq = happyDoSeq
-- otherwise it emits
-- 	happySeq = happyDontSeq

happyDoSeq, happyDontSeq :: a -> b -> b
happyDoSeq   a b = a `seq` b
happyDontSeq a b = b

-----------------------------------------------------------------------------
-- Don't inline any functions from the template.  GHC has a nasty habit
-- of deciding to inline happyGoto everywhere, which increases the size of
-- the generated parser quite a bit.

{-# LINE 312 "templates/GenericTemplate.hs" #-}
{-# NOINLINE happyShift #-}
{-# NOINLINE happySpecReduce_0 #-}
{-# NOINLINE happySpecReduce_1 #-}
{-# NOINLINE happySpecReduce_2 #-}
{-# NOINLINE happySpecReduce_3 #-}
{-# NOINLINE happyReduce #-}
{-# NOINLINE happyMonadReduce #-}
{-# NOINLINE happyGoto #-}
{-# NOINLINE happyFail #-}

-- end of Happy Template.
