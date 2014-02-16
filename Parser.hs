{-# OPTIONS_GHC -w #-}
module Parser where

import Lexer
import Syntax

-- parser produced by Happy Version 1.18.10

data HappyAbsSyn 
	= HappyTerminal (Token)
	| HappyErrorToken Int
	| HappyAbsSyn6 (Program)
	| HappyAbsSyn7 (Expression)
	| HappyAbsSyn8 ([Expression])
	| HappyAbsSyn10 (Command)

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
 action_37,
 action_38,
 action_39,
 action_40,
 action_41,
 action_42,
 action_43,
 action_44,
 action_45,
 action_46,
 action_47,
 action_48,
 action_49,
 action_50,
 action_51 :: () => Int -> ({-HappyReduction (HappyIdentity) = -}
	   Int 
	-> (Token)
	-> HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> (HappyIdentity) HappyAbsSyn)
	-> [HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> (HappyIdentity) HappyAbsSyn)] 
	-> HappyStk HappyAbsSyn 
	-> [(Token)] -> (HappyIdentity) HappyAbsSyn)

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
 happyReduce_13,
 happyReduce_14,
 happyReduce_15,
 happyReduce_16,
 happyReduce_17,
 happyReduce_18,
 happyReduce_19,
 happyReduce_20 :: () => ({-HappyReduction (HappyIdentity) = -}
	   Int 
	-> (Token)
	-> HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> (HappyIdentity) HappyAbsSyn)
	-> [HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> (HappyIdentity) HappyAbsSyn)] 
	-> HappyStk HappyAbsSyn 
	-> [(Token)] -> (HappyIdentity) HappyAbsSyn)

action_0 (28) = happyShift action_4
action_0 (6) = happyGoto action_19
action_0 _ = happyFail

action_1 (12) = happyShift action_10
action_1 (16) = happyShift action_11
action_1 (19) = happyShift action_12
action_1 (21) = happyShift action_13
action_1 (23) = happyShift action_14
action_1 (24) = happyShift action_15
action_1 (25) = happyShift action_16
action_1 (30) = happyShift action_17
action_1 (31) = happyShift action_18
action_1 (7) = happyGoto action_8
action_1 (8) = happyGoto action_9
action_1 _ = happyFail

action_2 (26) = happyShift action_6
action_2 (30) = happyShift action_7
action_2 (10) = happyGoto action_5
action_2 _ = happyFail

action_3 (28) = happyShift action_4
action_3 _ = happyFail

action_4 (30) = happyShift action_31
action_4 _ = happyFail

action_5 (22) = happyShift action_30
action_5 (32) = happyAccept
action_5 _ = happyFail

action_6 (12) = happyShift action_10
action_6 (16) = happyShift action_11
action_6 (19) = happyShift action_12
action_6 (21) = happyShift action_13
action_6 (23) = happyShift action_14
action_6 (24) = happyShift action_15
action_6 (25) = happyShift action_16
action_6 (30) = happyShift action_17
action_6 (31) = happyShift action_18
action_6 (7) = happyGoto action_29
action_6 (8) = happyGoto action_9
action_6 _ = happyFail

action_7 (20) = happyShift action_28
action_7 _ = happyFail

action_8 (11) = happyShift action_27
action_8 (32) = happyAccept
action_8 _ = happyFail

action_9 _ = happyReduce_13

action_10 (12) = happyShift action_10
action_10 (16) = happyShift action_11
action_10 (19) = happyShift action_12
action_10 (21) = happyShift action_13
action_10 (23) = happyShift action_14
action_10 (24) = happyShift action_15
action_10 (25) = happyShift action_16
action_10 (30) = happyShift action_17
action_10 (31) = happyShift action_18
action_10 (7) = happyGoto action_26
action_10 (8) = happyGoto action_9
action_10 _ = happyFail

action_11 (12) = happyShift action_10
action_11 (16) = happyShift action_11
action_11 (17) = happyShift action_25
action_11 (19) = happyShift action_12
action_11 (21) = happyShift action_13
action_11 (23) = happyShift action_14
action_11 (24) = happyShift action_15
action_11 (25) = happyShift action_16
action_11 (30) = happyShift action_17
action_11 (31) = happyShift action_18
action_11 (7) = happyGoto action_24
action_11 (8) = happyGoto action_9
action_11 _ = happyFail

action_12 (12) = happyShift action_10
action_12 (16) = happyShift action_11
action_12 (19) = happyShift action_12
action_12 (21) = happyShift action_13
action_12 (23) = happyShift action_14
action_12 (24) = happyShift action_15
action_12 (25) = happyShift action_16
action_12 (30) = happyShift action_17
action_12 (31) = happyShift action_18
action_12 (7) = happyGoto action_23
action_12 (8) = happyGoto action_9
action_12 _ = happyFail

action_13 _ = happyReduce_6

action_14 (12) = happyShift action_10
action_14 (16) = happyShift action_11
action_14 (19) = happyShift action_12
action_14 (21) = happyShift action_13
action_14 (23) = happyShift action_14
action_14 (24) = happyShift action_15
action_14 (25) = happyShift action_16
action_14 (30) = happyShift action_17
action_14 (31) = happyShift action_18
action_14 (7) = happyGoto action_22
action_14 (8) = happyGoto action_9
action_14 _ = happyFail

action_15 (12) = happyShift action_10
action_15 (16) = happyShift action_11
action_15 (19) = happyShift action_12
action_15 (21) = happyShift action_13
action_15 (23) = happyShift action_14
action_15 (24) = happyShift action_15
action_15 (25) = happyShift action_16
action_15 (30) = happyShift action_17
action_15 (31) = happyShift action_18
action_15 (7) = happyGoto action_21
action_15 (8) = happyGoto action_9
action_15 _ = happyFail

action_16 (12) = happyShift action_10
action_16 (16) = happyShift action_11
action_16 (19) = happyShift action_12
action_16 (21) = happyShift action_13
action_16 (23) = happyShift action_14
action_16 (24) = happyShift action_15
action_16 (25) = happyShift action_16
action_16 (30) = happyShift action_17
action_16 (31) = happyShift action_18
action_16 (7) = happyGoto action_20
action_16 (8) = happyGoto action_9
action_16 _ = happyFail

action_17 _ = happyReduce_7

action_18 _ = happyReduce_8

action_19 (32) = happyAccept
action_19 _ = happyFail

action_20 _ = happyReduce_11

action_21 _ = happyReduce_10

action_22 (11) = happyShift action_27
action_22 (12) = happyShift action_10
action_22 (16) = happyShift action_11
action_22 (19) = happyShift action_12
action_22 (21) = happyShift action_13
action_22 (23) = happyShift action_14
action_22 (24) = happyShift action_15
action_22 (25) = happyShift action_16
action_22 (30) = happyShift action_17
action_22 (31) = happyShift action_18
action_22 (7) = happyGoto action_42
action_22 (8) = happyGoto action_9
action_22 _ = happyFail

action_23 (11) = happyShift action_27
action_23 (12) = happyShift action_10
action_23 (16) = happyShift action_11
action_23 (19) = happyShift action_12
action_23 (21) = happyShift action_13
action_23 (23) = happyShift action_14
action_23 (24) = happyShift action_15
action_23 (25) = happyShift action_16
action_23 (30) = happyShift action_17
action_23 (31) = happyShift action_18
action_23 (7) = happyGoto action_41
action_23 (8) = happyGoto action_9
action_23 _ = happyFail

action_24 (11) = happyShift action_27
action_24 (17) = happyShift action_39
action_24 (18) = happyShift action_40
action_24 (9) = happyGoto action_38
action_24 _ = happyFail

action_25 _ = happyReduce_14

action_26 (11) = happyShift action_27
action_26 (13) = happyShift action_37
action_26 _ = happyFail

action_27 (12) = happyShift action_10
action_27 (16) = happyShift action_11
action_27 (19) = happyShift action_12
action_27 (21) = happyShift action_13
action_27 (23) = happyShift action_14
action_27 (24) = happyShift action_15
action_27 (25) = happyShift action_16
action_27 (30) = happyShift action_17
action_27 (31) = happyShift action_18
action_27 (7) = happyGoto action_36
action_27 (8) = happyGoto action_9
action_27 _ = happyFail

action_28 (12) = happyShift action_10
action_28 (16) = happyShift action_11
action_28 (19) = happyShift action_12
action_28 (21) = happyShift action_13
action_28 (23) = happyShift action_14
action_28 (24) = happyShift action_15
action_28 (25) = happyShift action_16
action_28 (30) = happyShift action_17
action_28 (31) = happyShift action_18
action_28 (7) = happyGoto action_35
action_28 (8) = happyGoto action_9
action_28 _ = happyFail

action_29 (11) = happyShift action_27
action_29 (27) = happyShift action_34
action_29 _ = happyFail

action_30 (26) = happyShift action_6
action_30 (30) = happyShift action_7
action_30 (10) = happyGoto action_33
action_30 _ = happyFail

action_31 (22) = happyShift action_32
action_31 _ = happyFail

action_32 (26) = happyShift action_6
action_32 (30) = happyShift action_7
action_32 (10) = happyGoto action_45
action_32 _ = happyFail

action_33 _ = happyReduce_18

action_34 (14) = happyShift action_44
action_34 _ = happyFail

action_35 (11) = happyShift action_27
action_35 _ = happyReduce_19

action_36 (11) = happyShift action_27
action_36 _ = happyReduce_5

action_37 _ = happyReduce_9

action_38 _ = happyReduce_15

action_39 _ = happyReduce_17

action_40 (12) = happyShift action_10
action_40 (16) = happyShift action_11
action_40 (19) = happyShift action_12
action_40 (21) = happyShift action_13
action_40 (23) = happyShift action_14
action_40 (24) = happyShift action_15
action_40 (25) = happyShift action_16
action_40 (30) = happyShift action_17
action_40 (31) = happyShift action_18
action_40 (7) = happyGoto action_43
action_40 (8) = happyGoto action_9
action_40 _ = happyFail

action_41 (11) = happyShift action_27
action_41 _ = happyReduce_12

action_42 (11) = happyShift action_27
action_42 _ = happyReduce_4

action_43 (11) = happyShift action_27
action_43 (17) = happyShift action_39
action_43 (18) = happyShift action_40
action_43 (9) = happyGoto action_48
action_43 _ = happyFail

action_44 (26) = happyShift action_6
action_44 (30) = happyShift action_7
action_44 (10) = happyGoto action_47
action_44 _ = happyFail

action_45 (22) = happyShift action_46
action_45 _ = happyFail

action_46 (26) = happyShift action_6
action_46 (29) = happyShift action_50
action_46 (30) = happyShift action_7
action_46 (10) = happyGoto action_33
action_46 _ = happyFail

action_47 (15) = happyShift action_49
action_47 (22) = happyShift action_30
action_47 _ = happyFail

action_48 _ = happyReduce_16

action_49 _ = happyReduce_20

action_50 (12) = happyShift action_10
action_50 (16) = happyShift action_11
action_50 (19) = happyShift action_12
action_50 (21) = happyShift action_13
action_50 (23) = happyShift action_14
action_50 (24) = happyShift action_15
action_50 (25) = happyShift action_16
action_50 (30) = happyShift action_17
action_50 (31) = happyShift action_18
action_50 (7) = happyGoto action_51
action_50 (8) = happyGoto action_9
action_50 _ = happyFail

action_51 (11) = happyShift action_27
action_51 _ = happyReduce_3

happyReduce_3 = happyReduce 7 6 happyReduction_3
happyReduction_3 ((HappyAbsSyn7  happy_var_7) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn10  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokenVar     p happy_var_2)) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn6
		 (Program happy_var_2 happy_var_4 happy_var_7
	) `HappyStk` happyRest

happyReduce_4 = happySpecReduce_3  7 happyReduction_4
happyReduction_4 (HappyAbsSyn7  happy_var_3)
	(HappyAbsSyn7  happy_var_2)
	_
	 =  HappyAbsSyn7
		 (Cons happy_var_2 happy_var_3
	)
happyReduction_4 _ _ _  = notHappyAtAll 

happyReduce_5 = happySpecReduce_3  7 happyReduction_5
happyReduction_5 (HappyAbsSyn7  happy_var_3)
	_
	(HappyAbsSyn7  happy_var_1)
	 =  HappyAbsSyn7
		 (Cons happy_var_1 happy_var_3
	)
happyReduction_5 _ _ _  = notHappyAtAll 

happyReduce_6 = happySpecReduce_1  7 happyReduction_6
happyReduction_6 _
	 =  HappyAbsSyn7
		 (Nil
	)

happyReduce_7 = happySpecReduce_1  7 happyReduction_7
happyReduction_7 (HappyTerminal (TokenVar     p happy_var_1))
	 =  HappyAbsSyn7
		 (Var happy_var_1
	)
happyReduction_7 _  = notHappyAtAll 

happyReduce_8 = happySpecReduce_1  7 happyReduction_8
happyReduction_8 (HappyTerminal (TokenInt     p happy_var_1))
	 =  HappyAbsSyn7
		 (mkInt happy_var_1
	)
happyReduction_8 _  = notHappyAtAll 

happyReduce_9 = happySpecReduce_3  7 happyReduction_9
happyReduction_9 _
	(HappyAbsSyn7  happy_var_2)
	_
	 =  HappyAbsSyn7
		 (happy_var_2
	)
happyReduction_9 _ _ _  = notHappyAtAll 

happyReduce_10 = happySpecReduce_2  7 happyReduction_10
happyReduction_10 (HappyAbsSyn7  happy_var_2)
	_
	 =  HappyAbsSyn7
		 (Hd happy_var_2
	)
happyReduction_10 _ _  = notHappyAtAll 

happyReduce_11 = happySpecReduce_2  7 happyReduction_11
happyReduction_11 (HappyAbsSyn7  happy_var_2)
	_
	 =  HappyAbsSyn7
		 (Tl happy_var_2
	)
happyReduction_11 _ _  = notHappyAtAll 

happyReduce_12 = happySpecReduce_3  7 happyReduction_12
happyReduction_12 (HappyAbsSyn7  happy_var_3)
	(HappyAbsSyn7  happy_var_2)
	_
	 =  HappyAbsSyn7
		 (IsEq happy_var_2 happy_var_3
	)
happyReduction_12 _ _ _  = notHappyAtAll 

happyReduce_13 = happySpecReduce_1  7 happyReduction_13
happyReduction_13 (HappyAbsSyn8  happy_var_1)
	 =  HappyAbsSyn7
		 (listToWhileList happy_var_1
	)
happyReduction_13 _  = notHappyAtAll 

happyReduce_14 = happySpecReduce_2  8 happyReduction_14
happyReduction_14 _
	_
	 =  HappyAbsSyn8
		 ([]
	)

happyReduce_15 = happySpecReduce_3  8 happyReduction_15
happyReduction_15 (HappyAbsSyn8  happy_var_3)
	(HappyAbsSyn7  happy_var_2)
	_
	 =  HappyAbsSyn8
		 (happy_var_2 : happy_var_3
	)
happyReduction_15 _ _ _  = notHappyAtAll 

happyReduce_16 = happySpecReduce_3  9 happyReduction_16
happyReduction_16 (HappyAbsSyn8  happy_var_3)
	(HappyAbsSyn7  happy_var_2)
	_
	 =  HappyAbsSyn8
		 (happy_var_2 : happy_var_3
	)
happyReduction_16 _ _ _  = notHappyAtAll 

happyReduce_17 = happySpecReduce_1  9 happyReduction_17
happyReduction_17 _
	 =  HappyAbsSyn8
		 ([]
	)

happyReduce_18 = happySpecReduce_3  10 happyReduction_18
happyReduction_18 (HappyAbsSyn10  happy_var_3)
	_
	(HappyAbsSyn10  happy_var_1)
	 =  HappyAbsSyn10
		 (Compos happy_var_1 happy_var_3
	)
happyReduction_18 _ _ _  = notHappyAtAll 

happyReduce_19 = happySpecReduce_3  10 happyReduction_19
happyReduction_19 (HappyAbsSyn7  happy_var_3)
	_
	(HappyTerminal (TokenVar     p happy_var_1))
	 =  HappyAbsSyn10
		 (Assign happy_var_1 happy_var_3
	)
happyReduction_19 _ _ _  = notHappyAtAll 

happyReduce_20 = happyReduce 6 10 happyReduction_20
happyReduction_20 (_ `HappyStk`
	(HappyAbsSyn10  happy_var_5) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn7  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn10
		 (While  happy_var_2 happy_var_5
	) `HappyStk` happyRest

happyNewToken action sts stk [] =
	action 32 32 notHappyAtAll (HappyState action) sts stk []

happyNewToken action sts stk (tk:tks) =
	let cont i = action i i tk (HappyState action) sts stk tks in
	case tk of {
	TokenConsInf p -> cont 11;
	TokenOpenBrc p -> cont 12;
	TokenClosBrc p -> cont 13;
	TokenOpenCur p -> cont 14;
	TokenClosCur p -> cont 15;
	TokenOpenSqu p -> cont 16;
	TokenClosSqu p -> cont 17;
	TokenComma   p -> cont 18;
	TokenIsEq    p -> cont 19;
	TokenAssign  p -> cont 20;
	TokenNil     p -> cont 21;
	TokenSemiCo  p -> cont 22;
	TokenConsPre p -> cont 23;
	TokenHead    p -> cont 24;
	TokenTail    p -> cont 25;
	TokenWhile   p -> cont 26;
	TokenDo      p -> cont 27;
	TokenRead    p -> cont 28;
	TokenWrite   p -> cont 29;
	TokenVar     p happy_dollar_dollar -> cont 30;
	TokenInt     p happy_dollar_dollar -> cont 31;
	_ -> happyError' (tk:tks)
	}

happyError_ 32 tk tks = happyError' tks
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

parseProg tks = happyRunIdentity happySomeParser where
  happySomeParser = happyThen (happyParse action_0 tks) (\x -> case x of {HappyAbsSyn6 z -> happyReturn z; _other -> notHappyAtAll })

parseExpr tks = happyRunIdentity happySomeParser where
  happySomeParser = happyThen (happyParse action_1 tks) (\x -> case x of {HappyAbsSyn7 z -> happyReturn z; _other -> notHappyAtAll })

parseComm tks = happyRunIdentity happySomeParser where
  happySomeParser = happyThen (happyParse action_2 tks) (\x -> case x of {HappyAbsSyn10 z -> happyReturn z; _other -> notHappyAtAll })

happySeq = happyDontSeq


parseError :: [Token] -> a
parseError []           = error "Parse error: reached end of file while parsing"
parseError (tok : rest) = error $ concat
    [ "Parse error: "
    , (tokStr tok)
    , " at line "
    , (show (lineNo tok))
    , ", char "
    , (show (charNo tok))
    ]

-- Takes a string representation of an integer and converts it into the
-- equivalent Expression representation
mkInt :: String -> Expression
mkInt s = intToExp (read s) Nil

-- Makes an Expression from an Int, using accumulating parameter style
intToExp :: Int -> Expression -> Expression
intToExp 0 acc = acc
intToExp n acc = intToExp (n - 1) (Cons Nil acc)

-- Convert a parsed list of Expressions into an actual while list
listToWhileList :: [Expression] -> Expression
listToWhileList (h:t) = Cons h (listToWhileList t)
listToWhileList []    = Nil
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
