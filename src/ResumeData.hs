module ResumeData where

import TeX

basicInfo ∷ Resume
basicInfo =
  paragraph
    [ pure "\\basicInfo{"
    , pure $ "\\email{joona.piirainen@gmail.com}" ++ period
    , -- , cn $ "\\phone{(+86) 180-8192-5082}" ++ period
      -- , en $ "\\phone{(1) 717-728-6526}" ++ period
      pure $ "\\github[japiirainen]{https://github.com/japiirainen}" ++ period
    , pure $ "\\homepage[japiirainen.xyz]{https://japiirainen.xyz}" ++ period
    , pure "\\linkedin[Joona Piirainen]{https://www.linkedin.com/in/joona-piirainen-a026351a8/}"
    , pure "}"
    ]
  where
    period = "~$\\circ$~"

reaktor ∷ Resume
reaktor =
  paragraph
    [ datedSection2 (date "2021" "05" ~~ date "-" "-") $
        paragraph $
          pure (en "\\textbf{Reaktor}, Helsinki, Finland")
    , paragraph $ pure (en "\\role{Software Engineering}{Software Developer}")
    , itemize
        [ en "\\item Worked at multiple client organizations solving difficult problems. Mostly in the domain of web development."
        , en "\\item Was involved in both senior and junior recruitment. Conducted both technical and non-technical interviews."
        , en "\\item Worked with a variety of technologies, such as \\texttt{React}, \\texttt{TypeScript}, \\texttt{Node.js}, \\texttt{Clojure}, \\texttt{ClojureScript}, \\texttt{PostgreSQL} and \\texttt{AWS}."
        , en "\\item Took part in the development of the company's internal tools."
        , en "\\item Attended internal and external trainings, such as \\texttt{Certified Scrum Master} and \\texttt{Consulting 101}."
        ]
    ]

puheet ∷ Resume
puheet =
  paragraph
    [ datedSection2 (date "2020" "10" ~~ date "2021" "05") $
        paragraph $
          pure (en "\\textbf{Puheet.com}, Espoo, Finland")
    , paragraph $ pure (en "\\role{Software Engineering}{Software Developer}")
    , itemize
        [ en "\\item Was responsible for the business critical platform components, such as the main backend system and multiple web frontend applications."
        , en "\\item Worked with a variety of technologies, such as \\texttt{React}, \\texttt{Vue}, \\texttt{TypeScript}, \\texttt{PHP}, \\texttt{Node.js}, \\texttt{MySQL}, \\texttt{ElasticSearch} \\texttt{Linux} and \\texttt{AWS}"
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
        [ en "\\item \\texttt{vl} is a tool for writing shell scripts in \\texttt{TypeScript}."
        , en "\\item It is powered by \\underline{\\href{https://deno.land/}{deno}}."
        ]
    ]

aoc ∷ Resume
aoc =
  paragraph
    [ datedSection (github "japiirainen/aoc-2022") (bold "Advent of Code") $
        paragraph $
          pure
            ( en "Solutions to \\underline{\\href{https://adventofcode.com/}{Advent of Code}}."
            )
    , itemize
        [ en "\\item I have participated in \\underline{\\href{https://adventofcode.com/}{Advent of Code}} since 2021."
        , en "\\item 2015 \\underline{\\href{https://github.com/japiirainen/aoc-2015}{solutions}}. (\\texttt{Clojure})"
        , en "\\item 2016 (WIP) \\underline{\\href{https://github.com/japiirainen/aoc-2016}{solutions}}. All days in different languages."
        , en "\\item 2020 \\underline{\\href{https://github.com/japiirainen/aoc-2020}{solutions}}. (\\texttt{Python})"
        , en "\\item 2021 \\underline{\\href{https://github.com/japiirainen/aoc-2021}{solutions}}. (\\texttt{Haskell} and \\texttt{Python})"
        , en "\\item 2022 \\underline{\\href{https://github.com/japiirainen/aoc-2022}{solutions}}. (\\texttt{Haskell})"
        ]
    ]

compiler ∷ Resume
compiler =
  paragraph
    [ itemTeX "Compilers"
    , en "Experience with parser generators, parser combinators and layout-sensitive syntax parsing."
    , en "Familiar with multiple evaluation strategies such as normalization by evaluation (NBE) and abstract machines such as CEK and CESK machines."
    ]
  where

programmingLanguages ∷ Resume
programmingLanguages =
  paragraph
    [ itemTeX "Program Languages"
    , en "\\textbf{multilingual} (not limited to any specific language),"
    , en $ "especially experienced in " ++ very ++ ","
    , en $ "comfortable with " ++ somehow ++ " (in random order)."
    ]
  where
    somehow = "\\texttt{Rust}, \\texttt{C}, \\texttt{Python}, \\texttt{Ocaml} and \\texttt{Agda}"
    very = "\\texttt{TypeScript}, \\texttt{Clojure}, \\texttt{Haskell} and \\texttt{C\\#}"

webFrameworks ∷ Resume
webFrameworks =
  paragraph
    [ itemTeX "Web Frameworks"
    , en $ "Very experienced in " ++ very ++ ","
    , en $ "comfortable with " ++ somehow ++ " (in random order)."
    ]
  where
    somehow = "\\texttt{dotnet} and \\texttt{FastAPI}"
    very = "\\texttt{React}, \\texttt{Next.js}, \\texttt{Servant} and \\texttt{Express.js}"

databases ∷ Resume
databases =
  paragraph
    [ itemTeX "Databases"
    , en $ "High degree of experience in " ++ very ++ ","
    , en $ "comfortable with " ++ somehow ++ " (in random order)."
    ]
  where
    somehow = "\\texttt{MySQL} and \\texttt{MongoDB}"
    very = "\\texttt{PostgreSQL}, \\texttt{Datomic} and \\texttt{ElasticSearch}"

functionalProgramming ∷ Resume
functionalProgramming =
  paragraph
    [ itemTeX "Functional Programming"
    , en "Extensive experience in functional programming. "
    , en "Understanding of both the theory and practical applications of functional programming."
    ]

typeTheory ∷ Resume
typeTheory =
  paragraph
    [ itemTeX "Type Theory"
    , en "Understanding of Martin-L\\\"{o}f type theory and Homotopy Type Theory."
    , en "Familiar with \\texttt{Idris}, \\texttt{Agda} and \\texttt{Lean4}"
    ]

cicd ∷ Resume
cicd =
  paragraph
    [ itemTeX "CI/CD"
    , en "Understanding of the importance of CI/CD in software development and quite a bit of experience in using and developing these systems."
    , en "Familiar with \\texttt{GitLab CI}, \\texttt{GitHub Actions}."
    ]

wow ∷ Resume
wow =
  paragraph
    [ itemTeX "Ways Of Working"
    , en "Skilled in Agile methodologies and can help teams thrive and make projects succeed."
    ]

skills ∷ Resume
skills =
  section "Skills" . pure $
    itemize
      [ programmingLanguages
      , webFrameworks
      , databases
      , functionalProgramming
      , compiler
      , typeTheory
      , cicd
      , wow
      ]
