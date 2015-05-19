{-# LANGUAGE RankNTypes #-}
import Text.Cassette
import Text.Cassette.Prim
import Text.Cassette.Lead
import Text.Cassette.Combinator
import Text.Cassette.Char
import Text.Cassette.Number

import Data.Char
import Data.Maybe
import System.IO hiding (print, parse)
import Data.Time
import System.TimeIt
import System.CPUTime
import Control.DeepSeq

start = getCurrentTime


data Exp = Var String
         | Con String
         | Add Exp Exp
         | Mul Exp Exp
         deriving Show

instance NFData Exp where
  rnf (Var s) = rnf s
  rnf (Con s) = rnf s
  rnf (Add e1 e2) = (rnf e1) `seq` (rnf e2) `seq` ()
  rnf (Mul e1 e2) = (rnf e1) `seq` (rnf e2) `seq` ()

 
data Stmt = Read String
          | Write Exp
          | Seq Stmt Stmt
          | If Exp Stmt Stmt
          | While Exp Stmt
          | Assign String Exp
          deriving Show

instance NFData Stmt where
  rnf (Read  s) = rnf s
  rnf (Write e) = rnf e
  rnf (Seq   s1 s2) = (rnf s1) `seq` (rnf s2) `seq` ()
  rnf (If  e s1 s2) = (rnf  e) `seq` (rnf s1) `seq` (rnf s2) `seq` ()
  rnf (While  e  s) = (rnf  e) `seq` (rnf  s) `seq` ()
  rnf (Assign s  e) = (rnf  s) `seq` (rnf  e) `seq` ()

varL = K7 leadout leadin where
  leadout k k' s x = k (\ s _ -> k' s x) s (Var x)
  leadin  k k' s t@(Var x)  = k (\ s _ -> k' s t) s x
  leadin  k k' s t          = k' s t

conL = K7 leadout leadin where
  leadout k k' s x = k (\ s _ -> k' s x) s (Con x)
  leadin  k k' s t@(Con x)  = k (\ s _ -> k' s t) s x
  leadin  k k' s t          = k' s t

addL = K7 leadout leadin where
  leadout k k' s t2 t1 = k (\ s _ -> k' s t2 t1) s (Add t1 t2)
  leadin  k k' s t@(Add t1 t2) = k (\ s _ _ -> k' s t) s t2 t1
  leadin  k k' s t             = k' s t

mulL = K7 leadout leadin where
  leadout k k' s t2 t1 = k (\ s _ -> k' s t2 t1) s (Mul t1 t2)
  leadin  k k' s t@(Mul t1 t2) = k (\ s _ _ -> k' s t) s t2 t1
  leadin  k k' s t             = k' s t

readL = K7 leadout leadin where
  leadout k k' s x = k (\ s _ -> k' s x) s (Read x)
  leadin  k k' s t@(Read x)  = k (\ s _ -> k' s t) s x
  leadin  k k' s t          = k' s t

writeL = K7 leadout leadin where
  leadout k k' s x = k (\ s _ -> k' s x) s (Write x)
  leadin  k k' s t@(Write x)  = k (\ s _ -> k' s t) s x
  leadin  k k' s t          = k' s t

seqL = K7 leadout leadin where
  leadout k k' s t2 t1 = k (\ s _ -> k' s t2 t1) s (Seq t1 t2)
  leadin  k k' s t@(Seq t1 t2) = k (\ s _ _ -> k' s t) s t2 t1
  leadin  k k' s t             = k' s t

whileL = K7 leadout leadin where
  leadout k k' s t2 t1 = k (\ s _ -> k' s t2 t1) s (While t1 t2)
  leadin  k k' s t@(While t1 t2) = k (\ s _ _ -> k' s t) s t2 t1
  leadin  k k' s t             = k' s t

assignL = K7 leadout leadin where
  leadout k k' s t2 t1 = k (\ s _ -> k' s t2 t1) s (Assign t1 t2)
  leadin  k k' s t@(Assign t1 t2) = k (\ s _ _ -> k' s t) s t2 t1
  leadin  k k' s t             = k' s t

ifL = K7 leadout leadin where
  leadout k k' s t3 t2 t1 = k (\ s _ -> k' s t3 t2 t1) s (If t1 t2 t3)
  leadin  k k' s t@(If t1 t2 t3) = k (\ s _ _ _ -> k' s t) s t3 t2 t1
  leadin  k k' s t             = k' s t


parens p = char '(' <> p <> char ')'

--manyPar p =  manyChar '(' <> manyChar ')'

--manyChar = many1 char

ident = many1 letter

alphaN = many alphaNum

dig = many1 digit

expr :: PP Exp
expr  =   mulL -->
          parens(expr <> optSpace <> string "*" <> optSpace <> expr)
      <|> addL --> 
          parens(expr <> optSpace <> string "+" <> optSpace <> expr)
      <|> conL --> 
          dig
      <|> varL --> 
          ident

optTrans ::  Bool -> PP0
optTrans True = unshift "\n" $ many (satisfy isSpace)
optTrans False = unshift " " $ many (satisfy isSpace)

stmnt :: PP Stmt
stmnt =   seqL    -->
          parens(stmnt<> optSpace <> string ";" <> optTrans True <> stmnt)
      <|> readL   -->
          string "read" <> parens(ident)
      <|> writeL  -->
          string "write" <> parens(optSpace <> expr <> optSpace)
      <|> whileL  -->
          string "while" <> parens(optSpace <> expr <> optSpace) <> optSpace <> string "{" <> 
          optSpace <> stmnt <> optSpace <> string "}"
      <|> assignL --> 
          ident <> optSpace <> string "=" <> optSpace <> expr   
      <|> ifL     -->
          {-expr <> stmnt <> stmnt-}
          string "if" <>  parens(optSpace <> expr <> optSpace) <> optSpace <> string "{" <> optTrans True <>   
          optSpace <> stmnt <> optSpace <> string "}" <> optSpace <> string "else" <> optSpace <> string "{" <>  
          optSpace <> stmnt <> optSpace <> string "}" 


stop = getCurrentTime

genTree 0 = Read "x"
genTree n = Seq (Read "x") (genTree (n-1))

fromPicoSeconds = \x -> (fromIntegral x) / (10.0 ^ 12)

{- Example to see

main = do
  beforeTreeBuildTime <- getCPUTime
  t <- return (genTree 10000)
  treeBuildTime <- deepseq t getCPUTime -- deepseq to get correct time of tree generation
  --treeBuildTime <- getCPUTime
  print $ fromPicoSeconds $ treeBuildTime - beforeTreeBuildTime
-}
  
prettyTest = do 
  t <- return (genTree 100000)
  _ <- deepseq t (return 0)
  start <- getCPUTime
  res <- return (pretty stmnt t)
  stop <- deepseq res getCPUTime
  print $ fromPicoSeconds $ stop - start

parseTest = do 
  t <- return (genTree 100000)
  s <- return (pretty stmnt t)
  _ <- deepseq s (return 5)
  start <- getCPUTime
  res <- return (parse stmnt (fromMaybe "" s))
  stop <- deepseq res getCPUTime
  print $ fromPicoSeconds $ stop - start
 

{-
main = do 
  src <- readFile "test.in"
  --result <- return $ fromMaybe "" ((operate src) >>= (pretty stmnt))
  --result2 <- return $ diffUTCTime stop start
  --writeFile "test.out" result
--  putStrLn result 
  print src
  timeIt $ print $ last $ show ((operate src) >>= (pretty stmnt))
--print $ diffUTCTime stop start
-}

operate :: String -> Maybe Stmt   
operate s = parse stmnt s;


-- | The type of binary leads, parameterized by the type of the left operand,
-- the right operand, and the type of the result.
--type BinL a b c =
--  forall r r'. K7 (C (c -> r))  (C (b -> a -> r))
--                  (C (c -> r')) (C (b -> a -> r'))

--addL --> (conL --> dig) <> sepSpace <> string "+" <> sepSpace <> term

--chainl :: PP0 -> BinL a a a -> PP a -> a -> PP a
--chainl opP opL xP dflt = chainl1 opP opL xP <|> shift dflt nothing      

--parse :: PP a -> String -> Maybe a
--parse csst = play csst (\_ _ x -> Just x) (const Nothing)

--pretty :: PP a -> a -> Maybe String
--pretty csst = play (flip csst) (const Just) (\_ _ -> Nothing) ""









