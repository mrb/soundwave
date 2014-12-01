module SoundwaveTest where

import Test.QuickCheck
import Soundwave
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Char8 as BC (pack, unpack)
import qualified Data.Map.Strict as M
import Data.Int
import qualified Data.Trie as T
import Test.HUnit

-- let name = (BL.fromStrict (BC.pack "foo"))
-- let vm = (M.fromList [(1::Int32,100::Int32)])
-- updateDB name vm (T.empty :: DB)
-- updateDB name (M.fromList [(1::Int32,1000::Int32)]) (updateDB name vm (T.empty :: DB))
