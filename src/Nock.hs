{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternSynonyms #-}

module Nock where

import Control.Monad
import Control.Monad.Except as Except
import Control.Monad.State as State

-- | Noun
data Noun = Cell2 Noun Noun | Atom Int
  deriving (Eq)

instance Show Noun where
  show (Cell2 a b) = "[" ++ show a ++ " " ++ go b ++ "]"
    where
      go (Cell2 a b) = show a ++ " " ++ go b
      go (Atom a) = show a
  show (Atom a) = show a

instance Read Noun where
  readsPrec d s = readsCell2 s ++ readsAtom s
    where
      readsCell2 :: ReadS Noun
      readsCell2 s = do
        ("[", s) <- lex s
        (a, s) <- reads s
        (" ", s) <- lex s
        (b, s) <- readsCell2' s
        ("]", s) <- lex s
        pure (Cell2 a b, s)

      readsCell2' :: ReadS Noun
      readsCell2' s =
        reads s
          ++ do
            (a, s) <- reads s
            (" ", s) <- lex s
            (b, s) <- readsCell2' s
            pure (Cell2 a b, s)

      readsAtom :: ReadS Noun
      readsAtom s = do
        (a, s) <- reads s
        pure (Atom a, s)

      -- need a custom lex that can capture whitespace
      lex :: ReadS String
      lex [] = pure ("", "")
      lex (c : s) = pure ([c], s)

pattern A0 = Atom 0

pattern A1 = Atom 1

pattern A2 = Atom 2

pattern A3 = Atom 3

pattern A4 = Atom 4

pattern A5 = Atom 5

pattern A6 = Atom 6

pattern A7 = Atom 7

pattern A8 = Atom 8

pattern A9 = Atom 9

pattern A10 = Atom 10

pattern A11 = Atom 11

pattern Cell3 a b c = Cell2 a (Cell2 b c)

pattern Cell4 a b c d = Cell2 a (Cell2 b (Cell2 c d))

pattern Cell5 a b c d e = Cell2 a (Cell2 b (Cell2 c (Cell2 d e)))

pattern Cell6 a b c d e f = Cell2 a (Cell2 b (Cell2 c (Cell2 d (Cell2 e f))))

-- | M
type M a = ExceptT String IO a

runM :: M a -> IO a
runM m =
  runExceptT m >>= \case
    Left err -> error err
    Right a -> pure a

_DEBUG = False

debug :: String -> M ()
debug = when _DEBUG . lift . putStrLn . ("[>] " ++)

-- | Instructions

{-
  ?a
  --------------------------------------------------
  ?[a b]              0
  ?a                  1
-}
wut :: Noun -> M Noun
wut n = do
  debug $ "?" ++ show n
  case n of
    Cell2 _a _b -> pure $ Atom 0
    Atom _a -> pure $ Atom 1

{-
+a
--------------------------------------------------
+[a b]              BOT
+a                  1 + a
-}
lus :: Noun -> M Noun
lus n = do
  debug $ "+" ++ show n
  case n of
    Atom a -> pure $ Atom (a + 1)
    n -> throwError $ "+" ++ show n

{-
=a
--------------------------------------------------
=[a a]              0
=[a b]              1
-}
tis :: Noun -> M Noun
tis n = do
  debug $ "=" ++ show n
  case n of
    Cell2 a b | a == b -> return $ Atom 0
    Cell2 a b | otherwise -> return $ Atom 1
    n -> throwError $ "=" ++ show n

{-
  /a
  --------------------------------------------------
  /[1 a]              a
  /[2 a b]            a
  /[3 a b]            b
  /[(a + a) b]        /[2 /[a b]]
  /[(a + a + 1) b]    /[3 /[a b]]
  /a                  BOT
-}
fas :: Noun -> M Noun
fas n = do
  debug $ "/" ++ show n
  case n of
    Cell2 A1 a -> pure a
    Cell3 A2 a _b -> pure a
    Cell3 A3 _a b -> pure b
    Cell2 (Atom a) b
      | even a,
        a' <- a `div` 2 ->
        fas . Cell2 A2 =<< fas (Cell2 (Atom a') b)
    Cell2 (Atom a) b
      | odd a,
        a' <- a `div` 2 ->
        fas . Cell2 A3 =<< fas (Cell2 (Atom a') b)
    n -> throwError $ "/" ++ show n

{-
  #a
  --------------------------------------------------
  #[1 a b]            a
  #[(a + a) b c]      #[a [b /[(a + a + 1) c]] c]
  #[(a + a + 1) b c]  #[a [/[(a + a) c] b] c]
  #a                  BOT
-}
edi :: Noun -> M Noun
edi n = do
  debug $ "#" ++ show n
  case n of
    Cell3 A1 a b -> pure a
    Cell3 (Atom a) b c
      | even a,
        a' <- a `div` 2 ->
        edi =<< Cell3 (Atom a') <$> (Cell2 b <$> fas (Cell2 (Atom (a + 1)) c)) <*> pure c
    Cell3 (Atom a) b c
      | odd a,
        a' <- a `div` 2 ->
        edi =<< Cell3 (Atom a') <$> (Cell2 <$> fas (Cell2 (Atom a') c) <*> pure b) <*> pure c
    n -> throwError $ "#" ++ show n

{-
  *a
  --------------------------------------------------
  *[a [b c] d]        [*[a b c] *[a d]]

  *[a 0 b]            /[b a]
  *[a 1 b]            b
  *[a 2 b c]          *[*[a b] *[a c]]
  *[a 3 b]            ?*[a b]
  *[a 4 b]            +*[a b]
  *[a 5 b c]          =[*[a b] *[a c]]

  *[a 6 b c d]        *[a *[[c d] 0 *[[2 3] 0 *[a 4 4 b]]]]
  *[a 7 b c]          *[*[a b] c]
  *[a 8 b c]          *[[*[a b] a] c]
  *[a 9 b c]          *[*[a c] 2 [0 1] 0 b]
  *[a 10 [b c] d]     #[b *[a c] *[a d]]

  *[a 11 [b c] d]     *[[*[a c] *[a d]] 0 3]
  *[a 11 b c]         *[a c]

  *a                  BOT
-}
tar :: Noun -> M Noun
tar n = do
  debug $ "*" ++ show n
  case n of
    -- [a [b c] d]        [*[a b c] *[a d]]
    Cell3 a (Cell2 b c) d -> Cell2 <$> tar (Cell3 a b c) <*> tar (Cell2 a d)
    --
    -- [a 0 b]            /[b a]
    Cell3 a A0 b -> fas (Cell2 b a)
    -- [a 1 b]            b
    Cell3 a A1 b -> pure b
    -- [a 2 b c]          *[*[a b] *[a c]]
    Cell4 a A2 b c -> tar =<< Cell2 <$> tar (Cell2 a b) <*> tar (Cell2 a c)
    -- [a 3 b]            ?*[a b]
    Cell3 a A3 b -> wut =<< tar (Cell2 a b)
    -- [a 4 b]            +*[a b]
    Cell3 a A4 b -> lus =<< tar (Cell2 a b)
    -- [a 5 b c]          =[*[a b] *[a c]]
    Cell4 a A5 b c -> tis =<< Cell2 <$> tar (Cell2 a b) <*> tar (Cell2 a c)
    -- [a 6 b c d]        *[a *[[c d] 0 *[[2 3] 0 *[a 4 4 b]]]]
    Cell5 a A6 b c d -> tar . Cell2 a =<< tar . Cell3 (Cell2 c d) A0 =<< tar . Cell3 (Cell2 A2 A3) A0 =<< tar (Cell4 a A4 A4 b)
    -- [a 7 b c]          *[*[a b] c]
    Cell4 a A7 b c -> tar =<< Cell2 <$> tar (Cell2 a b) <*> pure c
    -- [a 8 b c]          *[[*[a b] a] c]
    Cell4 a A8 b c -> tar =<< Cell2 <$> (Cell2 <$> tar (Cell2 a b) <*> pure a) <*> pure c
    -- [a 9 b c]          *[*[a c] 2 [0 1] 0 b]
    Cell4 a A9 b c -> tar =<< Cell5 <$> tar (Cell2 a c) <*> pure A2 <*> pure (Cell2 A0 A1) <*> pure A0 <*> pure b
    -- [a 10 [b c] d]     #[b *[a c] *[a d]]
    Cell4 a A10 (Cell2 b c) d -> edi =<< Cell3 b <$> tar (Cell2 a c) <*> tar (Cell2 a d)
    --
    -- [a 11 [b c] d]     *[[*[a c] *[a d]] 0 3]
    Cell4 a A11 (Cell2 b c) d -> tar =<< Cell3 <$> (Cell2 <$> tar (Cell2 a c) <*> tar (Cell2 a d)) <*> pure A0 <*> pure A3
    -- [a 11 b c]         *[a c]
    Cell4 a A11 b c -> tar (Cell2 a c)
    --  *a                  BOT
    n -> throwError $ "*" ++ show n

-- | Constructed Nouns
inc = read "[4 0 1]" :: Noun

dec = read "[8 [1 0] [8 [1 6 [5 [0 7] [4 0 6]] [0 6] [9 2 [[0 2] [4 0 6] [0 7]]]] [9 2 0 1]]]" :: Noun

-- | Utilities
applyM :: Noun -> Noun -> M Noun
applyM f a = tar (Cell2 a f)