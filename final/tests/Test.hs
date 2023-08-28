import           Control.Exception
import           Control.Monad
import           Test.Tasty
import           Common
import           Language.Elsa
import           Data.List (isInfixOf)
import qualified Trie                  as T
import qualified Types                 as P
import qualified Patterns              as P
import qualified Test.Tasty.QuickCheck as QC
import           System.FilePath

main :: IO ()
main = runTests 
  [ matryoshka
  , trie_manual
  , trie_props
  , patterns
  ]

-------------------------------------------------------------------------------
-- | Problem 1 ----------------------------------------------------------------
-------------------------------------------------------------------------------

matryoshka :: Score -> TestTree
matryoshka sc = testGroup "Problem 1"
  [ mkTest
      (check "Matryoshka.lc")
      "isz_zero"
      True
      "1.1: isz_zero"
    , mkTest
      (check "Matryoshka.lc")
      "isz_one"
      True
      "1.1: isz_one"
    , mkTest
      (check "Matryoshka.lc")
      "dec_two"
      True
      "1.2: dec_two"
    , mkTest
      (check "Matryoshka.lc")
      "dec_zero"
      True
      "1.2: dec_zero"
    , mkTest
      (check "Matryoshka.lc")
      "sub_three_two"
      True
      "1.3: sub_three_two"
    , mkTest
      (check "Matryoshka.lc")
      "sub_two_three"
      True
      "1.3: sub_two_three"
    , mkTest
      (check "Matryoshka.lc")
      "from_church_zero"
      True
      "1.4: from_church_zero"
    , mkTest
      (check "Matryoshka.lc")
      "from_church_one"
      True
      "1.4: from_church_one"
  ]
  where
    mkTest :: (Show b, Eq b) => (a -> IO b) -> a -> b -> String -> TestTree
    mkTest = mkTest' sc

    check :: FilePath -> Id -> IO Bool
    check f x = do
      r <- runElsaId (testDir </> f) x
      return (r == Just (OK (Bind x ())))

    testDir :: FilePath
    testDir = "src"

-------------------------------------------------------------------------------
-- | Problem 2 ----------------------------------------------------------------
-------------------------------------------------------------------------------

trie_manual :: Score -> TestTree
trie_manual sc = testGroup "Problem 2"
  [ mkTest
      (uncurry T.insert)
      ("he", T.empty)
      t0      
      "insert \"he\" empty"
  ,  mkTest
      (uncurry T.insert)
      ("it", t0)
      t2      
      "insert \"it\" (insert \"he\" empty)"
  ,  mkTest
      T.build
      ["it", "he"]
      t2
      "build [\"it\", \"he\"]"
  ,  mkTest
      T.build
      []
      (T.Node False [])
      "build []"  
  ,  mkTest
      (uncurry T.sameElems)
      ([1,2,3], [3,2,1,2])
      True
      "sameElems [1,2,3] [3,2,1,2]"
  ,  mkTest
      (uncurry T.sameElems)
      ([1,2,3], [1,2])
      False
      "sameElems [1,2,3] [1,2]"  
  ,  mkTest
      T.sortedTrie
      t4
      True
      "sortedTrie (Node False [('h',epsilon),('i',epsilon)])"
   ,  mkTest
      T.sortedTrie
      t5
      False
      "sortedTrie (Node False [('i',epsilon),('h',epsilon)])"
  ,  mkTest
      (uncurry T.withPrefixCorrect)
      ("h", T.pronounsSorted)
      True
      "withPrefixCorrect \"h\" pronounsSorted"
  ]
  where
    mkTest :: (Show b, Eq b) => (a -> b) -> a -> b -> String -> TestTree
    mkTest f = mkTest' sc (\x -> return (f x))

    t0 = T.Node False [('h',T.Node False [('e',T.Node True [])])]
    t2 = T.Node False [('h',T.Node False [('e',T.Node True [])]),('i',T.Node False [('t',T.Node True [])])]
    t4 = T.Node False [('h',T.Node True []),('i',T.Node True [])]
    t5 = T.Node False [('i',T.Node True []),('h',T.Node True [])]        

trie_props :: Score -> TestTree
trie_props sc = testGroup "Problem 2 (quickcheck)"
  [ scoreProp sc ("prop_contains_elts" ,       T.prop_contains_elts , 5) 
  , scoreProp sc ("prop_sorted" ,              T.prop_sorted , 5)
  , scoreProp sc ("prop_with_prefix_correct" , T.prop_with_prefix_correct , 5)
  ]

-------------------------------------------------------------------------------
-- | Problem 3 ----------------------------------------------------------------
-------------------------------------------------------------------------------
patterns :: Score -> TestTree
patterns sc = testGroup "Problem 3"
  [ mkTest
      (return 3 >>=)
      (\x -> return (x + 5))
      (P.Success 8)
      "do x <- return 3 ; return x + 5"
  , mkTest
      (P.Fail >>=)
      (\x -> return (x + 5))
      (P.Fail)
      "do x <- Fail ; return x + 5"
  , mkTest
      (P.match p1)
      v1
      (P.Success [("y", P.VInt 1), ("ys", vtail1)])
      "match `y:ys` against `[1,2,3]`"
  , mkTest
      (P.match p1)
      P.VNil
      P.Fail
      "match `y:ys` against `[]`"
  , fileTest  ( "tests/input/t1.hs"
              , P.VInt 1
              , 1 )
  , fileTest  ( "tests/input/t2.hs"
              , P.VInt 4
              , 1 )
  , fileTestE ( "tests/input/t3.hs"
              , "non-exhaustive"
              , 1 )

  ]
  where
    mkTest :: (Show b, Eq b) => (a -> b) -> a -> b -> String -> TestTree
    mkTest f = mkTest' sc (\x -> return (f x))

    fileTest (f, r, n)  = scoreTest' sc (P.execFile, f, r, n, "file: " ++ f)
    fileTestE (f, e, n) = scoreTest' sc (expectError e P.execFile, f, True, n, "file: " ++ f)

    p1 = P.PCons (P.PVar "y") (P.PVar "ys")
    vtail1 = P.VCons (P.VInt 2) (P.VCons (P.VInt 3) P.VNil)
    v1 = P.VCons (P.VInt 1) vtail1

expectError :: (Show b) => String -> (a -> IO b) -> a -> IO Bool
expectError err f x = do { r <- f x; print r; return False }
                      `catch`
                      (return . isInfixOf err . P.errMsg)
