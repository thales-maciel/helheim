{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Fast byte-level parser for 'FraudRequest', replacing Aeson on the HTTP hot
-- path. It is a positional recursive-descent parser over the known request
-- schema that produces results bit-identical to the Aeson instance:
--
--   * numbers go through 'Data.Scientific' exactly as Aeson does
--     ('Sci.toRealFloat' for Double, 'Sci.toBoundedInteger' for Int), so equal
--     JSON numbers yield equal Doubles/Ints;
--   * on ANYTHING it is not fully confident about (string escapes, an
--     unexpected key or field order, trailing bytes, malformed input) it bails
--     and falls back to the Aeson parser, which preserves the exact original
--     behaviour (including the 400 error path).
--
-- So a successful fast parse is equivalent to Aeson by construction, and every
-- other input is handled by Aeson. The parity tests assert the fast path both
-- triggers and matches Aeson across the real dataset.
module Helheim.RequestParser
  ( parseFraudRequest,
    runFast,
  )
where

import Data.Aeson (eitherDecodeStrict')
import qualified Data.ByteString as BS
import Data.ByteString.Unsafe (unsafeIndex)
import qualified Data.Scientific as Sci
import Data.Text (Text)
import qualified Data.Text.Encoding as TE
import Data.Word (Word8)
import Helheim.Types

-- | Parse a request body. Tries the fast path, falling back to Aeson on
-- anything the fast path does not handle (so behaviour matches Aeson exactly).
parseFraudRequest :: BS.ByteString -> Either String FraudRequest
parseFraudRequest bs =
  case runFast bs of
    Just req -> Right req
    Nothing -> eitherDecodeStrict' bs

-- | The fast path. 'Just' only when the body was fully parsed with confidence.
runFast :: BS.ByteString -> Maybe FraudRequest
runFast bs =
  case runP (fraudRequestP <* endOfInput) bs 0 of
    Just (req, _) -> Just req
    Nothing -> Nothing

-- Parser monad ------------------------------------------------------------

newtype P a = P {runP :: BS.ByteString -> Int -> Maybe (a, Int)}

instance Functor P where
  fmap f (P g) = P $ \bs i -> case g bs i of
    Just (a, i') -> Just (f a, i')
    Nothing -> Nothing

instance Applicative P where
  pure x = P $ \_ i -> Just (x, i)
  P f <*> P g = P $ \bs i -> case f bs i of
    Just (h, i') -> case g bs i' of
      Just (a, i'') -> Just (h a, i'')
      Nothing -> Nothing
    Nothing -> Nothing
  P f <* P g = P $ \bs i -> case f bs i of
    Just (a, i') -> case g bs i' of
      Just (_, i'') -> Just (a, i'')
      Nothing -> Nothing
    Nothing -> Nothing

instance Monad P where
  P g >>= f = P $ \bs i -> case g bs i of
    Just (a, i') -> runP (f a) bs i'
    Nothing -> Nothing

pfail :: P a
pfail = P $ \_ _ -> Nothing

-- Primitives --------------------------------------------------------------

isWs :: Word8 -> Bool
isWs w = w == 0x20 || w == 0x09 || w == 0x0A || w == 0x0D

isDigit :: Word8 -> Bool
isDigit w = w >= 0x30 && w <= 0x39

skipWsAt :: BS.ByteString -> Int -> Int
skipWsAt bs = go
  where
    len = BS.length bs
    go !i
      | i < len && isWs (unsafeIndex bs i) = go (i + 1)
      | otherwise = i

-- | Consume the given byte (after skipping whitespace), else fail.
sym :: Word8 -> P ()
sym w = P $ \bs i ->
  let j = skipWsAt bs i
   in if j < BS.length bs && unsafeIndex bs j == w
        then Just ((), j + 1)
        else Nothing

peekNonWs :: P (Maybe Word8)
peekNonWs = P $ \bs i ->
  let j = skipWsAt bs i
   in Just (if j < BS.length bs then Just (unsafeIndex bs j) else Nothing, j)

-- | Match a literal (after whitespace), e.g. @true@, @false@, @null@.
literal :: BS.ByteString -> P ()
literal s = P $ \bs i ->
  let j = skipWsAt bs i
   in if s `BS.isPrefixOf` BS.drop j bs
        then Just ((), j + BS.length s)
        else Nothing

endOfInput :: P ()
endOfInput = P $ \bs i ->
  let j = skipWsAt bs i
   in if j == BS.length bs then Just ((), j) else Nothing

-- | A JSON string. Bails (Nothing) on any backslash escape so escape semantics
-- are delegated to Aeson, and on invalid UTF-8.
stringP :: P Text
stringP = P $ \bs i0 ->
  let len = BS.length bs
      start0 = skipWsAt bs i0
   in if start0 < len && unsafeIndex bs start0 == 0x22
        then
          let content = start0 + 1
              scan !j
                | j >= len = Nothing
                | c == 0x22 =
                    case TE.decodeUtf8' (sliceBetween bs content j) of
                      Right t -> Just (t, j + 1)
                      Left _ -> Nothing
                | c == 0x5C = Nothing
                | otherwise = scan (j + 1)
                where
                  c = unsafeIndex bs j
           in scan content
        else Nothing

sliceBetween :: BS.ByteString -> Int -> Int -> BS.ByteString
sliceBetween bs from to = BS.take (to - from) (BS.drop from bs)

boolP :: P Bool
boolP = P $ \bs i ->
  let j = skipWsAt bs i
   in if "true" `BS.isPrefixOf` BS.drop j bs
        then Just (True, j + 4)
        else
          if "false" `BS.isPrefixOf` BS.drop j bs
            then Just (False, j + 5)
            else Nothing

-- | Parse a JSON number (after whitespace) into a 'Sci.Scientific' with the
-- exact same value Aeson would produce.
scientificP :: P Sci.Scientific
scientificP = P $ \bs i -> numberAt bs (skipWsAt bs i)

numberAt :: BS.ByteString -> Int -> Maybe (Sci.Scientific, Int)
numberAt bs i0
  | intEnd == intStart = Nothing -- need at least one integer digit
  | otherwise =
      case fracResult of
        Nothing -> Nothing
        Just (fracVal, fracLen, afterFrac) ->
          case expResult afterFrac of
            Nothing -> Nothing
            Just (expVal, afterExp) ->
              let intVal = digitsToInteger bs intStart intEnd
                  magnitude = intVal * (10 ^ fracLen) + fracVal
                  coeff = if neg then negate magnitude else magnitude
                  sci = Sci.scientific coeff (expVal - fracLen)
               in Just (sci, afterExp)
  where
    len = BS.length bs
    (neg, intStart) =
      if i0 < len && unsafeIndex bs i0 == 0x2D then (True, i0 + 1) else (False, i0)
    intEnd = spanDigits bs intStart

    fracResult :: Maybe (Integer, Int, Int)
    fracResult
      | intEnd < len && unsafeIndex bs intEnd == 0x2E =
          let fracStart = intEnd + 1
              fracEnd = spanDigits bs fracStart
           in if fracEnd == fracStart
                then Nothing -- '.' with no following digit
                else Just (digitsToInteger bs fracStart fracEnd, fracEnd - fracStart, fracEnd)
      | otherwise = Just (0, 0, intEnd)

    expResult :: Int -> Maybe (Int, Int)
    expResult pos
      | pos < len && (unsafeIndex bs pos == 0x65 || unsafeIndex bs pos == 0x45) =
          let (expNeg, signed) = case () of
                _
                  | pos + 1 < len && unsafeIndex bs (pos + 1) == 0x2D -> (True, pos + 2)
                  | pos + 1 < len && unsafeIndex bs (pos + 1) == 0x2B -> (False, pos + 2)
                  | otherwise -> (False, pos + 1)
              expEnd = spanDigits bs signed
           in if expEnd == signed
                then Nothing -- 'e' with no following digit
                else
                  let v = fromInteger (digitsToInteger bs signed expEnd)
                   in Just (if expNeg then negate v else v, expEnd)
      | otherwise = Just (0, pos)

spanDigits :: BS.ByteString -> Int -> Int
spanDigits bs = go
  where
    len = BS.length bs
    go !i
      | i < len && isDigit (unsafeIndex bs i) = go (i + 1)
      | otherwise = i

digitsToInteger :: BS.ByteString -> Int -> Int -> Integer
digitsToInteger bs from to = go from 0
  where
    go !i !acc
      | i >= to = acc
      | otherwise = go (i + 1) (acc * 10 + fromIntegral (unsafeIndex bs i - 0x30))

doubleP :: P Double
doubleP = Sci.toRealFloat <$> scientificP

intP :: P Int
intP = do
  s <- scientificP
  case Sci.toBoundedInteger s of
    Just n -> pure n
    Nothing -> pfail

-- Structure ---------------------------------------------------------------

-- | A @"key": value@ member where the key must match @expected@.
field :: Text -> P a -> P a
field expected valueP = do
  key <- stringP
  if key == expected
    then sym 0x3A {- ':' -} >> valueP
    else pfail

fraudRequestP :: P FraudRequest
fraudRequestP = do
  sym 0x7B {- '{' -}
  fid <- field "id" stringP
  sym 0x2C {- ',' -}
  tx <- field "transaction" transactionP
  sym 0x2C
  cust <- field "customer" customerP
  sym 0x2C
  merch <- field "merchant" merchantP
  sym 0x2C
  term <- field "terminal" terminalP
  sym 0x2C
  lastTx <- field "last_transaction" lastTransactionP
  sym 0x7D {- '}' -}
  pure (FraudRequest fid tx cust merch term lastTx)

transactionP :: P Transaction
transactionP = do
  sym 0x7B
  amount <- field "amount" doubleP
  sym 0x2C
  installments <- field "installments" intP
  sym 0x2C
  requestedAt <- field "requested_at" stringP
  sym 0x7D
  pure (Transaction amount installments requestedAt)

customerP :: P Customer
customerP = do
  sym 0x7B
  avgAmount <- field "avg_amount" doubleP
  sym 0x2C
  txCount <- field "tx_count_24h" intP
  sym 0x2C
  known <- field "known_merchants" stringArrayP
  sym 0x7D
  pure (Customer avgAmount txCount known)

merchantP :: P Merchant
merchantP = do
  sym 0x7B
  mid <- field "id" stringP
  sym 0x2C
  mcc <- field "mcc" stringP
  sym 0x2C
  avgAmount <- field "avg_amount" doubleP
  sym 0x7D
  pure (Merchant mid mcc avgAmount)

terminalP :: P Terminal
terminalP = do
  sym 0x7B
  isOnline <- field "is_online" boolP
  sym 0x2C
  cardPresent <- field "card_present" boolP
  sym 0x2C
  kmFromHome <- field "km_from_home" doubleP
  sym 0x7D
  pure (Terminal isOnline cardPresent kmFromHome)

lastTransactionP :: P (Maybe LastTransaction)
lastTransactionP = do
  next <- peekNonWs
  case next of
    Just 0x6E {- 'n' -} -> literal "null" >> pure Nothing
    Just 0x7B {- '{' -} -> Just <$> lastTransactionObjP
    _ -> pfail

lastTransactionObjP :: P LastTransaction
lastTransactionObjP = do
  sym 0x7B
  timestamp <- field "timestamp" stringP
  sym 0x2C
  kmFromCurrent <- field "km_from_current" doubleP
  sym 0x7D
  pure (LastTransaction timestamp kmFromCurrent)

-- | A JSON array of strings (handles the empty array).
stringArrayP :: P [Text]
stringArrayP = do
  sym 0x5B {- '[' -}
  next <- peekNonWs
  case next of
    Just 0x5D {- ']' -} -> sym 0x5D >> pure []
    _ -> do
      first <- stringP
      rest <- elements
      sym 0x5D
      pure (first : rest)
  where
    elements = do
      next <- peekNonWs
      case next of
        Just 0x2C {- ',' -} -> do
          _ <- sym 0x2C
          s <- stringP
          more <- elements
          pure (s : more)
        _ -> pure []
