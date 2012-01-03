\documentclass{article}
\usepackage{listings}
\lstloadlanguages{Haskell}
\lstnewenvironment{code}
    {\lstset{}%
      \csname lst@SetFirstLabel\endcsname}
    {\csname lst@SaveFirstLabel\endcsname}
    \lstset{
    showstringspaces=false,
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
The goal here is to use the \li{State} monad
to make one part of a simple parser.

The parser will take a list of strings.
\begin{code}
type Line = String
\end{code}
Each string
will have 0 or more spaces of indentation to begin.
The first has 0.
Each must match a previous indentation level.

The not-quite-parser returns a list of 
Strings or indentation-change tokens.

\begin{code}
data NestedLine = NestedLine String | Indent | Unindent
   deriving (Show)
\end{code}

To indent or unindent, the program must keep a record
of the indentations of previous lines... using 
a \li{State [Integer]} monad... 

\begin{code}
testStrings = [
   "pony",
   " unicorn",
   "   twilight sparkle",
   "   rarity",
   " pegasus",
   "  fluttershy",
   "dragon",
   "   Q"
   ]
expectOutput :: [NestedLine]
expectOutput = [
   NestedLine "pony",
   Indent,
   NestedLine "unicorn",
   Indent,
   NestedLine "twilight sparkle",
   NestedLine "rarity",
   Unindent,
   NestedLine "pegasus",
   Indent,
   NestedLine "fluttershy",
   Unindent,
   Unindent,
   NestedLine "dragon",
   Indent,
   NestedLine "Q"
   ]
\end{code}


\end{document}
