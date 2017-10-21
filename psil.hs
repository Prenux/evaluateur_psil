-- TP-2  --- Implantation d'une sorte de Lisp          -*- coding: utf-8 -*-
{-# OPTIONS_GHC -Wall #-}

-- Ce fichier défini les fonctionalités suivantes:
-- - Analyseur lexical
-- - Analyseur syntaxique
-- - Évaluateur
-- - Pretty printer

---------------------------------------------------------------------------
-- Importations de librairies et définitions de fonctions auxiliaires    --
---------------------------------------------------------------------------

import Text.ParserCombinators.Parsec -- Libraire d'analyse syntaxique (et lexicale).
import Data.Char        -- Conversion de Chars de/vers Int et autres
-- import Numeric       -- Pour la fonction showInt
import System.IO        -- Pour stdout, hPutStr
-- import Data.Maybe    -- Pour isJust and fromJust

---------------------------------------------------------------------------
-- La représentation interne des expressions de notre language           --
---------------------------------------------------------------------------
data Sexp = Snil                        -- La liste vide
          | Scons Sexp Sexp             -- Une paire
          | Ssym String                 -- Un symbole
          | Snum Int                    -- Un entier
          -- Génère automatiquement un pretty-printer et une fonction de
          -- comparaison structurelle.
          deriving (Show, Eq)

-- Exemples:
-- (+ 2 3)  ==  (((() . +) . 2) . 3)
--          ==>  Scons (Scons (Scons Snil (Ssym "+"))
--                            (Snum 2))
--                     (Snum 3)
--                   
-- (/ (* (- 68 32) 5) 9)
--     ==  (((() . /) . (((() . *) . (((() . -) . 68) . 32)) . 5)) . 9)
--     ==>
-- Scons (Scons (Scons Snil (Ssym "/"))
--              (Scons (Scons (Scons Snil (Ssym "*"))
--                            (Scons (Scons (Scons Snil (Ssym "-"))
--                                          (Snum 68))
--                                   (Snum 32)))
--                     (Snum 5)))
--       (Snum 9)

---------------------------------------------------------------------------
-- Analyseur lexical                                                     --
---------------------------------------------------------------------------

pChar :: Char -> Parser ()
pChar c = do { _ <- char c; return () }

-- Les commentaires commencent par un point-virgule et se terminent
-- à la fin de la ligne.
pComment :: Parser ()
pComment = do { pChar ';'; _ <- many (satisfy (\c -> not (c == '\n')));
                pChar '\n'; return ()
              }
-- N'importe quelle combinaison d'espaces et de commentaires est considérée
-- comme du blanc.
pSpaces :: Parser ()
pSpaces = do { _ <- many (do { _ <- space ; return () } <|> pComment); return () }

-- Un numbre entier est composé de chiffres.
integer     :: Parser Int
integer = do c <- digit
             integer' (digitToInt c)
          <|> do _ <- satisfy (\c -> (c == '-'))
                 n <- integer
                 return (- n)
    where integer' :: Int -> Parser Int
          integer' n = do c <- digit
                          integer' (10 * n + (digitToInt c))
                       <|> return n

-- Les symboles sont constitués de caractères alphanumériques et de signes
-- de ponctuations.
pSymchar :: Parser Char
pSymchar    = alphaNum <|> satisfy (\c -> c `elem` "!@$%^&*_+-=:|/?<>")
pSymbol :: Parser Sexp
pSymbol= do { s <- many1 (pSymchar);
              return (case parse integer "" s of
                        Right n -> Snum n
                        _ -> Ssym s)
            }

---------------------------------------------------------------------------
-- Analyseur syntaxique                                                  --
---------------------------------------------------------------------------

-- La notation "'E" est équivalente à "(quote E)"
pQuote :: Parser Sexp
pQuote = do { pChar '\''; pSpaces; e <- pSexp;
              return (Scons (Scons Snil (Ssym "quote")) e) }

-- Une liste (Tsil) est de la forme ( [e .] {e} )
pTsil :: Parser Sexp
pTsil = do pChar '('
           pSpaces
           (do { pChar ')'; return Snil }
            <|> do hd <- (do e <- pSexp
                             pSpaces
                             (do pChar '.'
                                 pSpaces
                                 return e
                              <|> return (Scons Snil e)))
                   pLiat hd)
    where pLiat :: Sexp -> Parser Sexp
          pLiat hd = do pChar ')'
                        return hd
                 <|> do e <- pSexp
                        pSpaces
                        pLiat (Scons hd e)

-- Accepte n'importe quel caractère: utilisé en cas d'erreur.
pAny :: Parser (Maybe Char)
pAny = do { c <- anyChar ; return (Just c) } <|> return Nothing

-- Une Sexp peut-être une liste, un symbol ou un entier.
pSexpTop :: Parser Sexp
pSexpTop = do { pTsil <|> pQuote <|> pSymbol
                <|> do { x <- pAny;
                         case x of
                           Nothing -> pzero
                           Just c -> error ("Unexpected char '" ++ [c] ++ "'")
                       }
              }

-- On distingue l'analyse syntaxique d'une Sexp principale de celle d'une
-- sous-Sexp: si l'analyse d'une sous-Sexp échoue à EOF, c'est une erreur de
-- syntaxe alors que si l'analyse de la Sexp principale échoue cela peut être
-- tout à fait normal.
pSexp :: Parser Sexp
pSexp = pSexpTop <|> error "Unexpected end of stream"

-- Une séquence de Sexps.
pSexps :: Parser [Sexp]
pSexps = do pSpaces
            many (do e <- pSexpTop
                     pSpaces
                     return e)

-- Déclare que notre analyseur syntaxique peut-être utilisé pour la fonction
-- générique "read".
instance Read Sexp where
    readsPrec _p s = case parse pSexp "" s of
                      Left _ -> []
                      Right e -> [(e,"")]

---------------------------------------------------------------------------
-- Sexp Pretty Printer                                                   --
---------------------------------------------------------------------------

showSexp' :: Sexp -> ShowS
showSexp' Snil = showString "()"
showSexp' (Snum n) = showsPrec 0 n
showSexp' (Ssym s) = showString s
showSexp' (Scons e1 e2) = showHead (Scons e1 e2) . showString ")"
    where
      showHead (Scons Snil e2') = showString "(" . showSexp' e2'
      showHead (Scons e1' e2') = showHead e1' . showString " " . showSexp' e2'
      showHead e = showString "(" . showSexp' e . showString " ."

-- On peut utiliser notre pretty-printer pour la fonction générique "show"
-- (utilisée par la boucle interactive de Hugs).  Mais avant de faire cela,
-- il faut enlever le "deriving Show" dans la déclaration de Sexp.
{-
instance Show Sexp where
    showsPrec p = showSexp'
-}

-- Pour lire et imprimer des Sexp plus facilement dans la boucle interactive
-- de Hugs:
readSexp :: String -> Sexp
readSexp = read
showSexp :: Sexp -> String
showSexp e = showSexp' e ""

---------------------------------------------------------------------------
-- Représentation intermédiaire L(ambda)exp(ression)                     --
---------------------------------------------------------------------------

type Var = String
type Tag = String
type Pat = Maybe (Tag, [Var])
data BindingType = Lexical | Dynamic
                   deriving (Show, Eq)
    
data Lexp = Lnum Int            -- Constante entière.
          | Lvar Var            -- Référence à une variable.
          | Llambda [Var] Lexp  -- Fonction anonyme prenant un argument.
          | Lapp Lexp [Lexp]    -- Appel de fonction, avec un argument.
          | Lcons Tag [Lexp]    -- Constructeur de liste vide.
          | Lcase Lexp [(Pat, Lexp)] -- Expression conditionelle.
          | Llet BindingType Var Lexp Lexp -- Déclaration de variable locale
          deriving (Show, Eq)

sconsToVarArr :: Sexp -> [Var]
sconsToVarArr (Scons Snil (Ssym x)) = [x]
sconsToVarArr (Scons (Scons a b) (Ssym x)) = (sconsToVarArr (Scons a b)) ++ [x]

getPat :: Sexp -> Pat
-- Not sure if gusta all the time... (i.e. : (Lapp (Llamba x y) z))
getPat (Scons Snil (Scons a b)) = (getPat (Scons a b))
-- got label, so create pattern
getPat (Scons Snil (Ssym a)) = Just (a,[])
-- label will be in the Scons, so append Ssym to [Var]
getPat (Scons (Scons a b) (Ssym c)) = 
    case (getPat (Scons a b)) of
    (Just (a,b)) -> Just (a,(b ++ c:[]))

getVar :: Sexp -> Var
getVar (Scons Snil (Ssym a)) = a
getVar (Scons a b) = getVar a

getVal :: Sexp -> Lexp
getVal (Scons )

-- Première passe simple qui analyse un Sexp et construit une Lexp équivalente.
s2l :: Sexp -> Lexp
s2l (Snum n) = Lnum n
s2l (Ssym s) = Lvar s

-- Slet
s2l (Scons (Scons (Scons Snil (Ssym "slet")) (Scons vars vals)) exp) = Llet Lexical (getVar vars) (Llambda (tail (sconsToVarArr vars)) (s2l vals)) (s2l exp)

-- Case
s2l (Scons (Scons Snil (Ssym "case")) a) = Lcase (s2l a) []

-- Generic lambda 
s2l (Scons (Scons (Scons Snil (Ssym "lambda")) x) y) = Llambda (sconsToVarArr x) (s2l y)

-- Cons
s2l (Scons (Scons Snil (Ssym "cons")) (Ssym a)) = Lcons a []

-- IF
s2l (Scons (Scons (Scons (Scons Snil (Ssym "if")) test) x) y) = Lcase (s2l test) [(Just ("true",[]), (s2l x)),(Just ("false",[]),(s2l y))]

-- Scons Snil a => sert seullement ajouter des parenthese autour
s2l (Scons Snil a) =
    case (s2l a) of
    (Lvar b)
-- appel de function, on rajoutera les args en remontant l'arbre
        | b `elem` ["+","-","*","/","<=","<",">=",">","="] -> Lapp (Lvar b) []
        | b == "cons" -> Lcons "" []
        | otherwise -> Lvar b
    (Llambda b c) -> Lapp (Llambda b c) []
-- Not sure if gusta for all case but worth a try
    (Lapp x y) -> Lapp x y

-- Scons Scons Sexp
s2l (Scons (Scons a b) c) = 
    case (s2l (Scons a b), c) of
-- ajoute args a Lapp 
    ((Lapp x y), _) -> Lapp x (y ++ (s2l c):[])
-- ajoute args au cons
    ((Lcons x y), _) -> Lcons x (y ++ (s2l c):[])
-- ajoute case to Lcase
    ((Lcase x y), (Scons u v)) -> Lcase x (y ++ ((getPat u), (s2l v)):[])


s2l se = error ("Malformed Sexp: " ++ (showSexp se))

---------------------------------------------------------------------------
-- Représentation du contexte d'exécution                                --
---------------------------------------------------------------------------

type Arity = Int

-- Type des valeurs manipulée à l'exécution.
data Value = Vnum Int
           | Vcons Tag [Value]
           | Vfun Arity (Env -> [Value] -> Value)

instance Show Value where
    showsPrec p (Vnum n) = showsPrec p n
    showsPrec p (Vcons tag vs) =
        let showTail [] = showChar ']'
            showTail (v : vs') =
                showChar ' ' . showsPrec p v . showTail vs'
        in showChar '[' . showString tag . showTail vs
    showsPrec _ (Vfun arity _)
        = showString ("<" ++ show arity ++ "-args-function>")

type Env = [(Var, Value)]

-- L'environnement initial qui contient les fonctions prédéfinies.
env0 :: Env
env0 = let false = Vcons "false" []
           true = Vcons "true" []
           mkbop (name, op) =
               (name, Vfun 2 (\ _ [Vnum x, Vnum y] -> Vnum (x `op` y)))
           mkcmp (name, op) =
               (name, Vfun 2 (\ _ [Vnum x, Vnum y]
                                  -> if x `op` y then true else false))
       in [("false", false),
           ("true", true)]
          ++ map mkbop
              [("+", (+)),
               ("*", (*)),
               ("/", div),
               ("-", (-))]
          ++ map mkcmp
              [("<=", (<=)),
               ("<", (<)),
               (">=", (>=)),
               (">", (>)),
               ("=", (==))]

---------------------------------------------------------------------------
-- Évaluateur                                                            --
---------------------------------------------------------------------------

eval :: Env -> Env -> Lexp -> Value
eval _senv _denv (Lnum n) = Vnum n

eval _senv [] (Lvar x) = 
--évaluation d'une variable lorsqu'aucune déclaration dynamique n'est active
        let a = 
	        case lookup x _senv of 
	        Nothing -> error "Variable not found"
		    Just a -> a
    	in a

eval _senv _denv (Lvar x) = 
--évaluation d'une variable.
	if (lookup x _denv) == Just v then v else 
	    let a = 
	        case lookup x _senv of 
	        Nothing -> error "Variable not found"
		    Just a -> a
    	in a


eval _senv _denv (Llambda vars exp) = 
--évaluation des lambda, pas encore complètement fonctionnel
    let
    varTags = (map Lvar vars)
    args = (map (eval _senv _denv) varTags)
    in Vfun (length vars) (\env args -> eval ((zip vars args) ++ env) _denv exp)


eval _senv _denv (Lapp op args) = 
--évaluation d'une fonction. Supporte uniquement les fonctions définies
--dans l'environnement statique présentement
	let fCons = eval _senv _denv op
	in case fCons of
	Vfun a f -> if (a == (length args)) then f _senv (map (eval _senv _denv) args) 
		else error ("incorrect number of arguments")

eval _senv _denv (Lcons tag args) = Vcons tag (map (eval _senv _denv) args) 

--si, lors de l'évaluation, on passe au travers de tous les patterns sans succès
--la liste des patterns est donc non-exhaustive donc on lace une erreur
eval _senv _denv (Lcase test []) = error ("Can't eval: non-exhaustive patterns in case statement")

eval _senv _denv (Lcase test (x:xs)) = 
        let pattern = eval _senv _denv test
        in case (pattern, fst x) of
--si le pattern est "_", on peut assumer que c'est le catch-all case, donc
--on évalue l'expression associée dès qu'on l'atteint
        (Vcons tag vals, Just ("_",_)) -> eval _senv _denv (snd x)
--si le tag et le nombre de valeurs sont conformes, on assume qu'on a trouvé le
--pattern recherché, donc on evalue l'expression associée à ce pattern
        (Vcons tag vals, Just (tag2, vars)) -> if (tag == tag2) && 
            ((length vals) == (length vars)) 
            then eval ((zip vars vals) ++ _senv) _denv (snd x) 
            else eval _senv _denv (Lcase test xs)

eval _senv _denv (Llet bind var val exp) =
        case bind of
        Lexical -> eval ((var,(eval _senv _denv val)):_senv) _denv exp
        Dynamic -> eval _senv ((var,(eval _senv _denv val)):_denv) exp

eval _ _ e = error ("Can't eval: " ++ show e)

---------------------------------------------------------------------------
-- Toplevel                                                              --
---------------------------------------------------------------------------

evalSexp :: Sexp -> Value
evalSexp = eval env0 [] . s2l

-- Lit un fichier contenant plusieurs Sexps, les évalues l'une après
-- l'autre, et renvoie la liste des valeurs obtenues.
run :: FilePath -> IO ()
run filename =
    do s <- readFile filename
       (hPutStr stdout . show)
           (let sexps s' = case parse pSexps filename s' of
                             Left _ -> [Ssym "#<parse-error>"]
                             Right es -> es
            in map evalSexp (sexps s))
