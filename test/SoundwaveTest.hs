module SoundwaveTest where

import Test.QuickCheck
import Soundwave
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Char8 as BC (pack, unpack)
import qualified Data.Map.Strict as M
import qualified Data.ByteString          as S
import qualified Data.ByteString.Internal as S (c2w, w2c)
import Data.Int
import qualified Data.Trie as T
import Test.HUnit
import Control.Monad
import Control.Applicative

-- let name1 = (BL.fromStrict (BC.pack "foo"))
-- let name2 = (BL.fromStrict (BC.pack "foob"))
-- let vm = (M.fromList [(1::Int32,100::Int32),(5::Int32,200::Int32)])
-- let vm2 = (M.fromList [(0::Int32,100::Int32),(5::Int32,900::Int32)])
-- let db = updateDB name2 vm2 (updateDB name1 vm (T.empty :: DB))
-- updateDB name1 (M.fromList [(1::Int32,1000::Int32)]) (updateDB name1 vm (T.empty :: DB))
-- let s = Snapshot{ dat=fromList (map (uncurry makeDatum) (T.toList db)) }

-- from https://github.com/aslatter/qc-instances/blob/master/src/Test/QuickCheck/Instances.hs
instance (Ord k, Arbitrary k, Arbitrary v) => Arbitrary (M.Map k v) where
    arbitrary = M.fromList <$> arbitrary
    shrink m = M.fromList <$> shrink (M.toList m)

-- from https://github.com/aslatter/qc-instances/blob/master/src/Test/QuickCheck/Instances.hs
instance Arbitrary BL.ByteString where
    arbitrary = BL.pack <$> arbitrary
    shrink xs = BL.pack <$> shrink (BL.unpack xs)

-- from http://code.haskell.org/~wren/bytestring-trie/test/Data/Trie/Test.hs
newtype Letter = Letter { unLetter :: Char }
    deriving (Eq, Ord, Show)

letters :: [Char]
letters = ['a'..'m']

instance Arbitrary Letter where
    arbitrary = Letter `fmap` elements letters

newtype Str = Str { unStr :: S.ByteString }
    deriving (Eq, Ord)

instance Show Str where
    show (Str s) = "Str {unStr = packC2W " ++ show s ++ " }"

packC2W :: String -> S.ByteString
packC2W  = S.pack . map S.c2w

instance Arbitrary Str where
    arbitrary = sized $ \n -> do
        k <- choose (0,n)
        s <- vector k
        c <- arbitrary -- We only want non-empty strings.
        return . Str . packC2W $ map unLetter (c:s)

instance (Arbitrary a) => Arbitrary (T.Trie a) where
    arbitrary = sized $ \n -> do
        k      <- choose (0,n)
        labels <- map unStr `fmap` vector k
        elems  <- vector k
        return . T.fromList $ zip labels elems
-- end of code from http://code.haskell.org/~wren/bytestring-trie/test/Data/Trie/Test.hs

-- sample (arbitrary :: Gen (T.Trie (M.Map Int32 Int32)))

