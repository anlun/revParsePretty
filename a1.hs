import Text.Cassette
import Text.Cassette.Prim
import Text.Cassette.Lead
import Text.Cassette.Combinator
import Text.Cassette.Char
import Text.Cassette.Number

data Term = Var String
          | Lam String Term
          | App Term Term
          deriving Show

varL = K7 leadout leadin where
  leadout k k' s x = k (\ s _ -> k' s x) s (Var x)
  leadin  k k' s t@(Var x)  = k (\ s _ -> k' s t) s x
  leadin  k k' s t          = k' s t

absL = K7 leadout leadin where
  leadout k k' s t' x = k (\ s _ -> k' s t' x) s (Lam x t')
  leadin  k k' s t@(Lam x t')  = k (\ s _ _ -> k' s t) s t' x
  leadin  k k' s t             = k' s t

appL = K7 leadout leadin where
  leadout k k' s t2 t1 = k (\ s _ -> k' s t2 t1) s (App t1 t2)
  leadin  k k' s t@(App t1 t2) = k (\ s _ _ -> k' s t) s t2 t1
  leadin  k k' s t             = k' s t


parens p = char '(' <> p <> char ')'

ident = many1 alphaNum

term :: PP Term
term  =   varL --> ident
      <|> absL --> char '|' <> ident <> string "." <> term
      <|> appL --> parens (term <> sepSpace <> term)
