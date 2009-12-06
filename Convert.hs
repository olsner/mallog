{-

Pre/Post-processing of mallog data. The main objective of this step is to
take input in the form of size+ptr (size == 0 means free) and convert it to
output of the form index+size (alloc) or index (free), where the index is the
index into an array of allocated pointers in the benchmark program.

This allows the benchmark program to run with minimal overhead from mapping
logged pointer values to the pointer values actually returned from the benched
malloc implementation.

-}

import Control.Applicative ((<$>))
import Control.Arrow
import Control.Monad
import Control.Monad.Instances
import Control.Monad.Writer (execWriter,tell)

import Control.Parallel.Strategies

import Data.Binary
import Data.Binary.Get
import Data.Binary.Put
import qualified Data.ByteString.Lazy as B
import Data.Int
import Data.List
import qualified Data.Map as M
import Data.Maybe
import Data.Ord

import System.IO

import Text.Printf

main = do
    res <- (`using` rnf) . process . parseIn . input <$> B.getContents
    res `seq` hPrintf stderr "Conversion done! %d operations.\n" (length res)
    B.putStr (output res)

data Action a = Alloc !a !a | Free !a
type Size = Word64
type Ptr = Word64

parseIn :: [(Word64,Word64)] -> [Action Word64]
parseIn = map (\(s,p) -> if s == 0 then Free p else Alloc (s `div` 2) p)

process :: (Eq a, Ord a, Num a, NFData a) => [Action a] -> [(Word64,a)]
process = execWriter . foldM f (-1,[],M.empty)
  where
    f (max,  fs,m) (Free p)    =
        case M.lookup p m of
            Just f -> tell [(f,0)] >> return (max, f:fs, M.delete p m)
            -- If we free an unknown pointer, just ignore the operation.
            Nothing -> return (max,fs,m)
    f (max,f:fs,m) (Alloc s p) = tell [(f,s)] >> return (max, fs, M.insert p f m)
    f (max,  [],m) (Alloc s p) = tell [(f,s)] >> return (f,   [], M.insert p f m) where f = max + 1

type ByteString = B.ByteString

maximumFst = maximum . map fst

toWord32 x = fromIntegral x :: Word32

output :: [(Word64,Word64)] -> ByteString
output xs = runPut $ do
    let max = maximumFst xs
    putWord64le . fromIntegral $ max
    let xs' = map (toWord32 *** toWord32) xs
    if max >= 2^32
        then mapM_ (\(i,a) -> putWord64le i >> putWord64le a) xs
        else mapM_ (\(i,a) -> putWord32le i >> putWord32le a) xs'

getAll :: Binary a => Get a -> ByteString -> [a]
getAll get = runGet getter
  where
    getter = isEmpty >>= \b -> if b then return [] else liftM2 (:) get getter

input = getAll (liftM2 (,) getWord64le getWord64le)
