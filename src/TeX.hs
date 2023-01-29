module TeX where

import Control.Applicative (Applicative (liftA2), liftA3)
import Control.Monad.Reader (MonadReader (ask), Reader)
import Data.Functor ((<&>))

data Mode
  = English
  | Elaborated
  deriving stock (Eq, Ord, Show)

fileName ∷ Mode → String
fileName English = "resume.tex"
fileName Elaborated = "resume-elab.tex"

-- | The Resume Monad.
type Resume = Reader Mode String

en ∷ String → Resume
en = pure

simple ∷ String → Resume
simple s =
  ask <&> \case
    English → s
    _ → []

noSimple ∷ Resume → Resume
noSimple s =
  ask >>= \case
    English → pure []
    _ → s

elab ∷ String → Resume
elab s =
  ask <&> \case
    Elaborated → s
    _ → []

paragraph ∷ [Resume] → Resume
paragraph [] = pure []
paragraph (r : rs) = f <$> r <*> paragraph rs
  where
    f "" b = b
    f a "" = a
    f a b = a ++ "\n" ++ b

tex ∷ String → String → Resume
tex n e = do
  pure $ "\\" ++ n ++ "{" ++ e ++ "}"

sec ∷ String → Resume
sec = tex "section"

url ∷ String → String
url s = "\\underline{\\url{" ++ s ++ "}}"

github ∷ Applicative f ⇒ String → f String
github s = pure $ "https://github.com/" ++ s

bold ∷ Applicative f ⇒ String → f String
bold s = pure $ "\\textbf{" ++ s ++ "}"

href ∷ String → String → String
href s t = "\\underline{\\href{" ++ s ++ "}{" ++ t ++ "}}"

section ∷ String → [Resume] → Resume
section e = paragraph . (sec e :)

date ∷ String → String → Resume
date y m = pure (m ++ "/" ++ y)

present ∷ Resume
present = pure "Present"

itemTeX ∷ String → Resume
itemTeX e =
  paragraph
    [ pure "\\item"
    , (++ ":") <$> tex "textbf" e
    ]

datedSection2 ∷ Resume → Resume → Resume
datedSection2 =
  liftA2
    (\x t → "\\datedsubsection{" ++ t ++ "}{" ++ x ++ "}")

datedSection ∷ Resume → Resume → Resume → Resume
datedSection =
  liftA3
    (\x t z → "\\datedsubsectionLinked{" ++ t ++ "~{\\small " ++ z ++ "}}{" ++ x ++ "}")

(~~) ∷ Resume → Resume → Resume
(~~) = liftA2 (\y z → y ++ " -- " ++ z)

itemize ∷ [Resume] → Resume
itemize rs =
  paragraph $
    [pure "\\begin{itemize}"]
      ++ rs
      ++ [pure "\\end{itemize}"]

enumerate ∷ [Resume] → Resume
enumerate rs =
  paragraph $
    [pure "\\begin{enumerate}"]
      ++ rs
      ++ [pure "\\end{enumerate}"]
