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

-- Special function for "Case" : Detects and defines a list without the need
-- for a constructor tag (cons)
casetagreader :: Sexp -> Lexp
casetagreader (Scons Snil (Ssym s)) = Lcons s []
casetagreader (Scons rest var) = case casetagreader rest of
    Lcons tag undervars -> Lcons tag (undervars ++ [(s2l var)])
    _ -> error ("Not a tagged list")
casetagreader (Ssym "_") = Lvar "_"
casetagreader _ = error ("Not a tagged list or default branching indicator")

-- Première passe simple qui analyse un Sexp et construit une Lexp équivalente.
s2l :: Sexp -> Lexp
s2l (Snum n) = Lnum n
s2l (Ssym s) = Lvar s

-- Single function static declaration
s2l (Scons (Scons (Scons Snil (Ssym "slet")) 
         (Scons Snil (Scons (Scons Snil (Ssym s))
         (Scons (Scons (Scons Snil (Ssym "lambda")) args) lambfunc)))) func) =
    let lambdas2l = 
            s2l (Scons (Scons (Scons Snil (Ssym "lambda")) args) lambfunc)
    in Llet Lexical s (lambdas2l) (s2l func)

-- Single function dynamic declaration
s2l (Scons (Scons (Scons Snil (Ssym "dlet")) 
         (Scons Snil (Scons (Scons Snil (Ssym s))
         (Scons (Scons (Scons Snil (Ssym "lambda")) args) lambfunc)))) func) =
    let lambdas2l = 
            s2l (Scons (Scons (Scons Snil (Ssym "lambda")) args) lambfunc)
    in Llet Dynamic s (lambdas2l) (s2l func)

-- Single variable static declaration
s2l (Scons (Scons (Scons Snil (Ssym "slet")) 
         (Scons Snil (Scons (Scons Snil (Ssym s)) (def)))) (func)) =
    Llet Lexical s (s2l def) (s2l func)

-- Single variable dynamic declaration
s2l (Scons (Scons (Scons Snil (Ssym "dlet")) 
         (Scons Snil (Scons (Scons Snil (Ssym s)) (def)))) (func)) =
    Llet Dynamic s (s2l def) (s2l func)

-- Sugary version of single variable static declaration
s2l (Scons (Scons (Scons Snil (Ssym "slet")) 
         (Scons Snil (Scons (Scons Snil (sugardec)) sugarfunc))) func) =
    let Lapp symf args = s2l sugardec
        translvar (Lvar x) = x
        translvar _ = error ("Not a variable")
        lambdafunc = Llambda (map translvar args) (s2l sugarfunc)
    in Llet Lexical (translvar symf) lambdafunc (s2l func)

-- Sugary version of single variable dynamic declaration
s2l (Scons (Scons (Scons Snil (Ssym "dlet")) 
         (Scons Snil (Scons (Scons Snil (sugardec)) sugarfunc))) func) =
    let Lapp symf args = s2l sugardec
        translvar (Lvar x) = x
        translvar _ = error ("Not a variable")
        lambdafunc = Llambda (map translvar args) (s2l sugarfunc)
    in Llet Dynamic (translvar symf) lambdafunc (s2l func)


-- Multiple variable static declaration
s2l (Scons (Scons (Scons Snil (Ssym "dlet"))
         (Scons (undercond) lastdef)) func) =
    --let underlet = 
      --      s2l (Scons (Scons (Scons Snil (Ssym "dlet")) undercond) func)
    s2l (Scons (Scons (Scons Snil (Ssym "dlet")) undercond)
        (Scons (Scons (Scons Snil (Ssym "dlet")) (Scons Snil lastdef)) func))

-- Multiple variable dynamic declaration
s2l (Scons (Scons (Scons Snil (Ssym "slet"))
         (Scons (undercond) (lastdef))) func) =
    s2l (Scons (Scons (Scons Snil (Ssym "slet")) undercond)
        (Scons (Scons (Scons Snil (Ssym "slet")) (Scons Snil lastdef)) func))


-- Sugary version of case (if) -- Needs to be given true and false choices
s2l (Scons (Scons (Scons (Scons Snil (Ssym "if")) (condit)) (a)) (b)) =
    s2l (Scons (Scons (Scons (Scons Snil (Ssym "case")) (condit))
            (Scons (Scons Snil (Scons Snil (Ssym "true"))) (a))) 
            (Scons (Scons Snil (Scons Snil (Ssym "false"))) (b)))

-- Case formula with one possibility
-- "_" represents a default case and should be written last
s2l (Scons (Scons (Scons Snil (Ssym "case")) (searched)) 
             (Scons (Scons Snil (verif)) (func))) = case casetagreader verif of
        Lcons constverif varverif ->
            let remlvar (Lvar x) = x
                remlvar _ = error ("Not variable names...")
                varlist = map (remlvar) varverif
            in  Lcase (s2l searched) [(Just (constverif, varlist), s2l func)]
        Lvar "_" -> Lcase (s2l searched) [(Nothing, s2l func)]
        _ -> error ("Malformed case")


-- Empty list construction (empty list)
s2l (Scons (Scons Snil (Ssym "cons")) (Ssym name)) =
    Lcons name []

-- Base case for a lambda function (one variable)
s2l (Scons (Scons (Scons Snil (Ssym "lambda")) (Scons Snil (Ssym x))) (y)) =
    let Lvar varx = s2l (Ssym x)
    in Llambda [varx] (s2l y)

-- Lambda function with multiple cariables
s2l (Scons (Scons (Scons Snil (Ssym "lambda")) 
                                 (Scons (x) (y))) (func)) =
    let Llambda a _ = (s2l (Scons (Scons (Scons Snil (Ssym "lambda")) 
                                                            (x)) (func)))
        Lvar vary = (s2l y)
        arglist = (a ++ [vary])
    in Llambda arglist (s2l func)

-- Calling a function given one value
s2l (Scons (Scons Snil lambdafunc) val) = --case s2l val of
    Lapp (s2l lambdafunc) [s2l val]

-- Some special cases...
s2l (Scons (restapp) val) = case s2l restapp of
    -- Calling a function given multiple values
    Lapp underfunc underval -> 
        Lapp underfunc (underval ++ [s2l val])
    -- Multiple values structure
    Lcons name underval -> Lcons name (underval ++ [s2l val])
    -- Multi "Case"
    Lcase underexp underlist -> 
        let (Scons (Scons Snil (verif)) (func)) = val
        in  case casetagreader verif of
            Lcons constverif varverif -> 
                -- Detecting variables
                let remlvar (Lvar x) = x
                    remlvar _ = error ("Not variable names...")
                    varlist = map (remlvar) varverif
                in  Lcase underexp (underlist ++ 
                                [(Just (constverif, varlist), s2l func)])
            -- Default case
            Lvar "_" -> Lcase underexp (underlist ++ [(Nothing, s2l func)])
            _ -> error ("Malformed case")
    _ -> error ("Operation undefined")

-- ¡¡ COMPLETER !!
s2l se = error ("Malformed Sexp: " ++ (showSexp se))

---------------------------------------------------------------------------
-- Représentation du contexte d'exécution                                --
---------------------------------------------------------------------------

type Arity = Int

-- Type des valeurs manipulée à l'exécution.
data Value = Vnum Int
           | Vvar Var -- Used for transfer of variables in lambda evaluation
           | Vcons Tag [Value]
           | Vfun Arity Env [Var] (Env -> [Value] -> Value)
           -- Env is added for closure and Var for passing of variables

instance Show Value where
    showsPrec p (Vnum n) = showsPrec p n
    showsPrec p (Vvar n) = showsPrec p n
    showsPrec p (Vcons tag vs) =
        let showTail [] = showChar ']'
            showTail (v : vs') =
                showChar ' ' . showsPrec p v . showTail vs'
        in showChar '[' . showString tag . showTail vs
    showsPrec _ (Vfun arity _ _ _)
        = showString ("<" ++ show arity ++ "-args-function>")

type Env = [(Var, Value)]

-- L'environnement initial qui contient les fonctions prédéfinies.
env0 :: Env
env0 = let false = Vcons "false" []
           true = Vcons "true" []
           mkbop (name, op) =
               (name, Vfun 2 [] [] (\ _ [Vnum x, Vnum y] -> Vnum (x `op` y)))
           mkcmp (name, op) =
               (name, Vfun 2 [] [] (\ _ [Vnum x, Vnum y]
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

-- Multiple calls in a row
eval _senv _denv (Lapp (Lapp func valx) valy) =
    eval _senv _denv (Lapp func (valx ++ valy))

-- Lambda function composition
eval _senv _denv (Llambda varx (Llambda vary func)) =
    -- Might create unwanted duplicates...
    let removedup [] = []
        removedup (a : arest) = (a : (removedup (filter (/= a) arest)))
    in eval _senv _denv (Llambda (removedup (varx ++ vary)) func)

-- Call of a lambda function composition
eval _senv _denv (Lapp (Llambda varx (Llambda vary func)) vals) =
    let removedup [] = []
        removedup (a : arest) = (a : (removedup (filter (/= a) arest)))
    in eval _senv _denv (Lapp (Llambda (removedup (varx ++ vary)) func) vals)

-- Declarations of variables and functions
eval senvx denvx (Llet lexDyn expr (def) (func)) = case lexDyn of
    Dynamic -> case lookup expr denvx of
      Nothing ->
        let 
            _denv = [(expr, eval _senv _denv def)] ++ denvx
            _senv = senvx
        in eval _senv _denv func
      Just _ ->
        let 
            filtenv :: Var -> Env -> Env
            filtenv _ [] = []
            filtenv a ((b, bval) : brest) = 
                if a == b then filtenv a brest
                else [(b, bval)] ++ filtenv a brest
            denvxtemp = filtenv expr denvx
            _denv = [(expr, eval senvx denvx def)] ++ denvxtemp
            _senv = filtenv expr senvx
        in eval _senv _denv func
    Lexical -> 
        let 
            _senv = [(expr, eval senvx denvx def)] ++ senvx
            _denv = denvx
        in eval _senv _denv func

-- Special function call where the function has no variable
eval _senv _denv (Lapp (Lnum x) _ ) = Vnum x

-- Evaluating a declared function or variable
eval _senv _denv (Lvar x) = case lookup x _senv of
    Nothing -> case lookup x _denv of
        Nothing -> Vvar x  
        Just y -> y
    Just y -> y

-- Evaluation of a lambda function call
eval senv denv (Lapp (Llambda vars lambdafunc) vals) =
    -- Right number of arguments given...
    if (length vars) == (length vals) then
        let valsprep = map (eval senv denv) vals
            -- Combines variables with corresponding values
            comb [] [] = []
            comb (x : xrest) (y : yrest) = [(x, y)] ++ comb xrest yrest
            comb _ _ = []
            combenv = comb vars valsprep ++ senv
            newdenv = denv
        in eval combenv newdenv lambdafunc
    else 
        -- Not enough arguments given (superior order functions)
        if (length vars) > (length vals) then
            let valsprep = map (eval senv denv) vals
                comb [] [] = []
                comb (x : xrest) (y : yrest) = [(x, y)] ++ comb xrest yrest
                comb _ _ = []
                combenv = comb vars valsprep ++ senv
                newdenv = denv
                -- Separates remaining unknown variables
                restvar [] rest = rest
                restvar (_ : arest) (_ : brest) = restvar arest brest
                restvar _ _ = []
            in eval combenv newdenv
                    (Llambda (restvar vals vars) lambdafunc)
        else -- Too many arguments are given to a function...
            error (show lambdafunc) 

-- Function call made on a variable (non-lambda)
eval senv denv (Lapp (Lvar s) vals) = case eval senv denv (Lvar s) of
  Vfun _ envf vars func -> 
    let valsprep = map (eval senv denv) vals
        comb [] [] = []
        comb (a : arest) (b : brest) = [(a, b)] ++ (comb arest brest)
        comb _ _ = []
        newenv = comb vars valsprep
             -- No variables : Special case for predefined functions...
    in  if length vars == 0 then func (denv) valsprep
        else 
            -- Some unknown variables found...
            -- We only take the values that are needed
            let 
                restval rest [] = rest
                restval (_ : arest) (_ : brest) = restval arest brest
                restval _ _ = []
            in  func (newenv ++ envf ++ denv) (restval valsprep vars)
  -- Special case where the value found is already numerical
  Vnum x -> Vnum x
  _ -> error ("   ")

-- Special case : function call in front of a declaration
eval _senv _denv (Lapp (Llet dynlex var def func) val) =
    eval _senv _denv (Llet dynlex var def (Lapp func val))

-- Evaluating other function calls (Lcons / Lcase)
eval _senv _denv (Lapp (underfunc) vals) = case eval _senv _denv underfunc of
    Vfun nb envf vars func -> 
        -- Regular case
        if (length vals) == nb then 
            let valsprep = map (eval _senv _denv) vals
                comb [] [] = []
                comb (a : arest) (b : brest) = [(a, b)] ++ (comb arest brest)
                comb _ _ = []
                newenv = comb vars valsprep
            in func (newenv ++ envf ++ _denv) valsprep 
        else 
            -- Too many arguments are given to a function
            if (length vals) > nb then 
                error ("Too many arguments were given to a function")
            else
                -- Too few arguments are given to a function
                error ("Not enough arguments were given to a function")
    Vnum x -> Vnum x
    _ -> error ("Not a function")

-- Evaluation of a "Case"
eval _senv _denv (Lcase (searched) explist) = 
        -- Has to be made on a structure
    let Vcons searchedtag vallist = eval _senv _denv searched 
    in 
        case explist of
            -- Develops list of possibilities
            ((verify, func) : rest) -> 
                case verify of
                    -- Special default case (_)
                    Nothing -> eval _senv _denv func
                    -- Otherwise comparing...
                    Just (tag, vars) -> 
                        if tag == searchedtag then
                            -- Found a match... We assign values to variables
                            if length vallist == 0 then
                                eval _senv _denv func
                            else 
                                let comb [] [] = []
                                    comb (a : []) (b : []) = [(a, b)]
                                    comb (a : arest) (b : brest) = 
                                               [(a, b)] ++ comb arest brest
                                    comb _ _ = []
                                    newsenv = (comb vars vallist) ++ _senv
                                in eval newsenv _denv func
                        -- Not a match... We look further in the list
                        else eval _senv _denv (Lcase (searched) rest) 
            _ -> error (show explist)

-- Evaluation a lambda function
eval _senv _denv (Llambda vars func) = 
    let nb = length vars
        -- Transforms values to be assigned
        transval (Vnum x) = Lnum x
        transval _ = error ("Non numeric arguments given")
    -- We use a Lapp in case we want to assign values later
    in Vfun nb _senv vars (\ newenv vals -> (eval newenv _denv
                                         (Lapp func (map transval vals))))

-- Evaluating a structure
eval _senv _denv (Lcons name vals) = Vcons name (map (eval _senv _denv) vals)


-- ¡¡ COMPLETER !!
--eval _ _ e = error ("Can't eval: " ++ show e)

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
