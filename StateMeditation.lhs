\documentclass{article}
\usepackage{listings}
\lstloadlanguages{Haskell}
\lstnewenvironment{code}
    {\lstset{}%
      \csname lst@SetFirstLabel\endcsname}
    {\csname lst@SaveFirstLabel\endcsname}
    \lstset{
    frame=single,
    language=Haskell,
      basicstyle=\small\ttfamily,
      flexiblecolumns=false,
      basewidth={0.5em,0.45em},
      literate={+}{{$+$}}1 {/}{{$/$}}1 {*}{{$*$}}1 {=}{{$=$}}1
               {>}{{$>$}}1 {<}{{$<$}}1 {\\}{{$\lambda$}}1
               {\\\\}{{\char`\\\char`\\}}1
               {->}{{$\rightarrow$}}2 {>=}{{$\geq$}}2 {<-}{{$\leftarrow$}}2
               {<=}{{$\leq$}}2 {=>}{{$\Rightarrow$}}2 
               {\ .}{{$\circ$}}2 {\ .\ }{{$\circ$}}2
               {>>}{{>>}}2 {>>=}{{>>=}}2
               {|}{{$\mid$}}1               
    }
\renewcommand{\familydefault}{\sfdefault}
\renewcommand{\rmdefault}{phv} % Arial
\renewcommand{\sfdefault}{phv} % Arial
\newcommand{\li}{\lstinline}

\begin{document}

I want to understand the \lstinline{State} monad. 
I find it's a lot more complicated than \lstinline{Maybe} or \lstinline{List}.

Monads, it is generally accepted, are burritos. The \li{State} monad is a burrito 
with an innovative new option:
bake an ingredient of your choice into the tortilla...



\section{Basics}
The type of any state monad involves two types, one for the state and one for the 
value...

\begin{code}
{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, ScopedTypeVariables #-}


import Control.Monad.State

x :: State Int Int
x = do
   thing <- get
   put (thing+1)
   return thing

\end{code}

The same can be written without do-notation...
\begin{code}
x' :: State Int Int
x' = get >>= (\thing -> (put (thing+1) >> return thing))
\end{code}

What's that \lstinline{get} function?
\begin{code}%ignore
get :: MonadState s m => m s
\end{code}
What's that mean? Once it's parameterized, it becomes clear.
\begin{code}
get' :: State Int Int
get' = get

get'' :: State [Double] [Double]
get'' = get
\end{code}
But it's a little constrained, compared to other \lstinline{State} monad types we can parameterize.

\begin{code}%type error
get''' :: State Double Int
get''' = get -- type error
\end{code}
\lstinline{s} and \lstinline{a} must be the same type.




The type of \lstinline{get} is \lstinline{State Int Int}, but \lstinline{put} takes an \lstinline{Int}, not the monad.
\begin{code}% borken
x'' :: State Int Int
x'' = (put $ get + 1) >> (return get) --type error
\end{code}



To ``get out,'' use \lstinline{execState} and \lstinline{evalState}.
Both take a \lstinline{State s a} monad and an initial state \lstinline{s}. This is confusing
because the documentation (and concept) refers to both the monad \lstinline{State s a} and
the naked value \lstinline{s} as ``state.'' \lstinline{State s a} is something different!

\lstinline{execState} returns the final (non-monadic) state \lstinline{s}.
\lstinline{evalState} returns \lstinline{a}.

\begin{code}
y :: Int
y = execState (withState (+1) x) 1

z :: State Int Int
z = withState (+1) x

y' = execState z 1
\end{code}


\lstinline{mapState} lets one apply a non-monadic function to a \lstinline{State s a}.

What is the difference between (\lstinline{mapState f}) and (\lstinline{liftM f})??

This begins with a dinky little function. It takes a pair of values,
one being the value (\lstinline{evalState}) and one being the state (\lstinline{execState}).
\begin{code}
merp :: (Int, String) -> (Int, String)
merp (x, str) = (x+1, str ++ " --> x was " ++ (show x))
\end{code}


We also describe a new \lstinline{State} monad here...

\begin{code}
aOne :: State String Int
aOne = do
   aString <- get
   put $ aString ++ " has been merped"
   return $ length aString
\end{code}

Here's a new \lstinline{State} monad of the same type (\lstinline{State String Int})
that's derived from the original \lstinline{aOne}.
But remember we haven't actually provided an initial state yet.
We're just describing what we \emph{might} do, if we're ever given an initial state.
\begin{code}
aTwo :: State String Int
aTwo = mapState merp aOne
\end{code}

\lstinline{execState} and \lstinline{evalState} provide a way to put in that initial state
\begin{code}
aEval :: String -> Int
aEval = evalState aTwo
aExec :: String -> String
aExec = execState aTwo
\end{code}

Now, (\lstinline{execState aOne}) takes a \lstinline{String} and returns a \lstinline{String}. So we can sequence them...
\begin{code}%demo
aExec "one"
-- "one has been merped --> x was 3"

chainingExampleOne :: String
chainingExampleOne = 
   execState aTwo $ execState aOne  $ execState aOne "ONE"
   -- "ONE has been merped has been merped has been merped --> x was 35"

\end{code}

But there's an easy way to sequence any monad! What happens if we sequence (\lstinline{State String Int})s?
\begin{code}
chainingExampleTwo = execState (sequence [aOne, aOne, aTwo]) "TWO"
   -- "TWO has been merped has been merped has been merped --> x was 35"
\end{code}
So these are two ways of writing the same thing.

Well, not exactly the same.
\begin{code}
chainingExampleOneA :: (Int, String)
chainingExampleOneA =
   runState aTwo $ execState aOne  $ execState aOne "ONE-A"
   -- (38,"ONE-A has been merped has been merped has been merped --> x was 37")

chainingExampleTwoA :: ([Int], String)
chainingExampleTwoA = runState (sequence [aOne, aOne, aTwo]) "TWO-A"
   -- ([5,21,38],"TWO-A has been merped has been merped has been merped --> x was 37")
\end{code}
\lstinline{sequence} produces a new monad around an array. 
Not so when ``chaining by hand.''

\lstinline{withState} is the counterpart of \lstinline{mapState} 
and \li{liftM}.

\li{liftM} handles the value (\lstinline{Int}), \li{withState} handles the internal state (String),
and \li{mapState} handles both at once.
\begin{code}
merp2 :: String -> String
merp2 str = str ++ " and an ice cream cone"

w :: State String Int
w = withState merp2 aOne
\end{code}

\section{Recursion and State}

\section{Nitpicking}
This example, from the \lstinline{Control.Monad.State} documentation, felt contrived when I first met it. 
Because the anonymous function here does nothing at all with the table,
mapState could have been replaced with \lstinline{liftM}.
\begin{code}
--theirs
sumNumberedTree :: (Eq a) => Tree a -> State (Table a) Int
sumNumberedTree = mapState (\ (t, tab) -> (sumTree t, tab))  . numberTree

--mine
sumNumberedTree' :: (Eq a) => Tree a -> State (Table a) Int
sumNumberedTree' = liftM  sumTree . numberTree 
\end{code}

A better example of \lstinline{mapState}, then, would be something that considers both \li{s} and \li{a}. 
This one demands that the state and value of the \lstinline{State} monad be the same.
\begin{code}
swapState :: State x x -> State x x
swapState = mapState (\ (t, tab) -> (tab, t)) 
\end{code}



\begin{code}
type Line = String
data IndentedLine = IndentedLine Int Line
   deriving (Show)
data IndentationLevel = IndentationLevel {spaces :: Int,
                                          level  :: Int}
   deriving (Show)

type IndentationHistory = [IndentationLevel]

instance Show (State IndentationHistory [IndentedLine]) where
   show indentation  = "x = State IndentationHistory [IndentedLine], where for example\n\trunState x 0 ->\n\t" ++ (show $ runState indentation [IndentationLevel 0 0]) 

indentToLevels :: [Line] -> State IndentationHistory [IndentedLine]
indentToLevels [] = do 
   return []
indentToLevels (line:lines) = do
   oldHistory <- get
   let currentSpaces = spaces currentS
   let currentLevel = level currentS
   let newLevel = case (compare currentSpaces (leadingSpaces line)) of
                     LT -> currentLevel + 1
                     EQ -> currentLevel 
                     GT -> currentLevel - 1
   put $ oldHistory ++ [IndentationLevel {spaces = (leadingSpaces line), level = newLevel}]
   theRest <- indentToLevels lines
   return $ (IndentedLine newLevel line) : theRest
       
leadingSpaces line = length $ takeWhile (==' ') line

matchHistory indentationHistory indentation = case matchingHistory of
   where matchingHistory = dropWhile (not . indentEquals indentation) indentationHistory

testStrings = ["Derp de herp",
   "herp",
   " pony",
   "   twilight sparkle",
   "   rarity",
   "  fluttershy",
   " dragon",
   "concluded"]


\end{code}


Helper code reproduced from the tutorial.
\begin{code}
data Tree a = Nil | Node a (Tree a) (Tree a) deriving (Show, Eq)
type Table a = [a]
numberTree :: Eq a => Tree a -> State (Table a) (Tree Int)
numberTree Nil = return Nil
numberTree (Node x t1 t2) 
     =  do num <- numberNode x
           nt1 <- numberTree t1
           nt2 <- numberTree t2
           return (Node num nt1 nt2)
  where 
  numberNode :: Eq a => a -> State (Table a) Int
  numberNode x
     = do table <- get
          (newTable, newPos) <- return (nNode x table)
          put newTable
          return newPos
  nNode::  (Eq a) => a -> Table a -> (Table a, Int)
  nNode x table
     = case (findIndexInList (== x) table) of
       Nothing -> (table ++ [x], length table)
       Just i  -> (table, i)
  findIndexInList :: (a -> Bool) -> [a] -> Maybe Int
  findIndexInList = findIndexInListHelp 0
  findIndexInListHelp _ _ [] = Nothing
  findIndexInListHelp count f (h:t)
     = if (f h)
       then Just count
       else findIndexInListHelp (count+1) f t

testTree = Node "Zero" (Node "One" (Node "Two" Nil Nil) (Node "One" (Node "Zero" Nil Nil) Nil)) Nil

sumTree :: (Num a) => Tree a -> a
sumTree Nil = 0
sumTree (Node e t1 t2) = e + (sumTree t1) + (sumTree t2)

\end{code}


Oh \LaTeX I can't quit you.

\end{document}
