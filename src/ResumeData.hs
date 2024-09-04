module ResumeData where

import TeX

basicInfo ∷ Resume
basicInfo =
  paragraph
    [ pure "\\basicInfo{"
    , pure $ "\\email{japiirainen@proton.me}" ++ period
    , -- , pure $ "\\phone{+348405482488}" ++ period
      pure $ "\\github[japiirainen]{https://github.com/japiirainen}" ++ period
    , pure $ "\\homepage[japiirainen.xyz]{https://japiirainen.xyz}" ++ period
    , pure "\\linkedin[Joona Piirainen]{https://www.linkedin.com/in/joona-piirainen-a026351a8/}"
    , pure "}"
    ]
  where
    period = "~$\\circ$~"

adalyon ∷ Resume
adalyon =
  paragraph
    [ datedSection2 (date "2024" "01" ~~ date "-" "-") $
        paragraph $
          pure (en "\\textbf{Adalyon}, Helsinki, Finland")
    , paragraph $ pure (en "\\role{Software Engineering}{Software Developer}")
    , itemize
        [ en "\\item Part of a team building a behavioural analytics platform from the ground up."
        , en "\\item Combined different maching learning techniques to accurately measure team dynamics."
        , en "\\item Worked accross the whole technology stack, including e.g. data engineering, web development and iOS development."
        , en "\\item Worked with technologies such as \\texttt{pytorch}, \\texttt{coreml}, \\texttt{python}, \\texttt{fastapi}, \\texttt{Microsoft Azure}, \\texttt{swift}, \\texttt{c++} and \\texttt{typescript}."
        ]
    ]

reaktor ∷ Resume
reaktor =
  paragraph
    [ datedSection2 (date "2021" "05" ~~ date "2024" "01") $
        paragraph $
          pure (en "\\textbf{Reaktor}, Helsinki, Finland")
    , paragraph $ pure (en "\\role{Software Engineering}{Software Developer}")
    , itemize
        [ en "\\item Collaborated with numerous clients to resolve business-critical technical issues."
        , en $ "\\item Extensive experience helping critical software projects succeed in the " ++ defsec ++ " domain."
        , en "\\item Part of both junior and senior recruitment teams. Conducted both technical and non-technical interviews."
        , en "\\item Worked with a variety of technologies, such as \\texttt{React}, \\texttt{TypeScript}, \\texttt{Node.js}, \\texttt{Clojure}, \\texttt{Java}, \\texttt{ClojureScript}, \\texttt{PostgreSQL} and \\texttt{AWS}."
        , en "\\item Got to take part in many trainings, such as \\texttt{Cyber Security}, \\texttt{Certified Scrum Master} and \\texttt{Consulting 101}."
        ]
    ]
  where
    defsec = href "https://www.reaktor.com/services/industries/defense-and-security" "Defence and Security"

puheet ∷ Resume
puheet =
  paragraph
    [ datedSection2 (date "2020" "10" ~~ date "2021" "05") $
        paragraph $
          pure (en "\\textbf{Puheet.com}, Espoo, Finland")
    , paragraph $ pure (en "\\role{Software Engineering}{Software Developer}")
    , itemize
        [ en "\\item Was responsible for business critical platform components, such as the main backend system and multiple web frontend applications."
        , en "\\item Worked with a variety of technologies, such as \\texttt{React}, \\texttt{Vue}, \\texttt{TypeScript}, \\texttt{PHP}, \\texttt{Node.js}, \\texttt{MySQL}, \\texttt{ElasticSearch} \\texttt{Linux} and \\texttt{AWS}."
        ]
    ]

fp ∷ Resume
fp =
  paragraph
    [ datedSection (github "japiirainen/fp") (bold "fp") $
        paragraph $
          pure
            ( en "A small, weird and unpractical programming language."
            )
    , itemize
        [ en "\\item \\texttt{fp} is a programming language heavily inspired by the language John Backus described in his 1977 Turing Award lecture."
        , en "\\item based on a \\underline{\\href{https://dl.acm.org/doi/10.1145/359576.359579}{paper}} by John Backus."
        ]
    ]

vl ∷ Resume
vl =
  paragraph
    [ datedSection (github "japiirainen/vl") (bold "vl") $
        paragraph $
          pure
            ( en "Shell scripting in \\texttt{TypeScript}."
            )
    , itemize
        [ en "\\item \\texttt{vl} is a tool for writing shell scripts in \\texttt{TypeScript}. It is powered by \\underline{\\href{https://deno.land/}{deno}}."
        ]
    ]

aoc ∷ Resume
aoc =
  paragraph
    [ datedSection (github "japiirainen/aoc-2023") (bold "aoc") $
        paragraph $
          pure
            ( en "Solutions to \\underline{\\href{https://adventofcode.com/}{Advent of Code}} problems in many programming languages."
            )
    , itemize
        [ en $ "\\item " ++ year 2015 ++ ", " ++ year 2016 ++ ", " ++ year 2020 ++ ", " ++ year 2021 ++ ", " ++ year 2022 ++ ", " ++ year 2023 ++ "."
        ]
    ]
  where
    year ∷ Int → String
    year n = "\\underline{\\href{https://github.com/japiirainen/aoc-" ++ show n ++ "}{" ++ show n ++ "}}"

compiler ∷ Resume
compiler =
  paragraph
    [ itemTeX "Compilers"
    , en "Experience with parser generators, parser combinators and layout-sensitive syntax parsing."
    , en "Familiar with multiple evaluation strategies such as normalization by evaluation (NBE) and abstract machines such as CEK and CESK machines."
    ]

programmingLanguages ∷ Resume
programmingLanguages =
  paragraph
    [ itemTeX "Program Languages"
    , en "\\textbf{multilingual} (not limited to any specific language),"
    , en $ "especially experienced in " ++ very ++ ","
    , en $ "comfortable with " ++ somehow ++ " (in random order)."
    ]
  where
    somehow = "\\texttt{Rust}, \\texttt{C}, \\texttt{C++}, \\texttt{Java} and \\texttt{Ocaml}"
    very = "\\texttt{TypeScript}, \\texttt{Clojure}, \\texttt{Haskell}, \\texttt{Python} and \\texttt{C\\#}"

webFrameworks ∷ Resume
webFrameworks =
  paragraph
    [ itemTeX "Web Technologies"
    , en $ "Extensive experienced working with " ++ very ++ "."
    ]
  where
    very = "\\texttt{React}, \\texttt{Next.js}, \\texttt{FastAPI}, \\texttt{dotnet}, \\texttt{Express.js}."

paradigms ∷ Resume
paradigms =
  paragraph
    [ itemTeX "Programming Paradigms"
    , en "Deep understanding of ins and outs of different programming paradigms, such as functional, object oriented, logic and array programming."
    , en "Understand that different paradigms fit different problems, and that there are no silver bullets."
    ]

practices ∷ Resume
practices =
  paragraph
    [ itemTeX "Programming Practices"
    , en "I value in high quality software and I always strive to find better ways to deliver it faster and more reliably.."
    ]

cloud ∷ Resume
cloud =
  paragraph
    [ itemTeX "Cloud Technologies"
    , en "Extensive experience with clouds techologies such as \\texttt{Microsoft Azure} and \\texttt{AWS}."
    , en "Experience with developing and maintaining infrastructure pipelines using the \\texttt{Infrastructure as Code} methodology."
    ]

wow ∷ Resume
wow =
  paragraph
    [ itemTeX "Ways Of Working"
    , en "Love helping teams thrive and make projects succeed. Extensive experience of facilitating activities such as retros and project kickoffs."
    ]

skills ∷ Resume
skills =
  section "Skills" . pure $
    itemize
      [ wow
      , programmingLanguages
      , webFrameworks
      , paradigms
      , cloud
      , practices
      , compiler
      ]
