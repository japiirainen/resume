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
        , en "\\item Combined different maching learning techniques to accurately assess team dynamics."
        , en "\\item Worked across the entire technology stack, including data engineering, web development, and iOS development."
        , en "\\item Worked with technologies such as \\texttt{PyTorch}, \\texttt{CoreML}, \\texttt{Python}, \\texttt{FastAPI}, \\texttt{Microsoft Azure}, \\texttt{Swift}, \\texttt{C++} and \\texttt{TypeScript}."
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
        [ en "\\item Responsible for business critical platform components, such as the main backend system and multiple web frontend applications."
        , en "\\item Worked with a variety of technologies, such as \\texttt{React}, \\texttt{Vue}, \\texttt{TypeScript}, \\texttt{PHP}, \\texttt{Node.js}, \\texttt{MySQL}, \\texttt{ElasticSearch} \\texttt{Linux} and \\texttt{AWS}."
        ]
    ]

fp ∷ Resume
fp =
  paragraph
    [ datedSection (github "japiirainen/fp") (bold "fp") $
        paragraph $
          pure
            ( en "A small, weird and unpractical \\texttt{programming language}."
            )
    , itemize
        [ en "\\item \\texttt{fp} is an unconventional programming language inspired by a \\underline{\\href{https://dl.acm.org/doi/10.1145/359576.359579}{paper}} by John Backus. John Backus described the language in his 1977 Turing Award lecture."
        , en "\\item For more information and numerous examples, please refer to the project's \\underline{\\href{https://github.com/japiirainen/fp}{README.md}}."
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
        [ en "\\item \\texttt{vl} is a tool for writing shell scripts in \\texttt{TypeScript}, powered by \\underline{\\href{https://deno.land/}{deno}}."
        ]
    ]

aoc ∷ Resume
aoc =
  paragraph
    [ datedSection (github "japiirainen/aoc-2023") (bold "aoc") $
        paragraph $
          pure
            ( en "Solutions to \\underline{\\href{https://adventofcode.com/}{Advent of Code}} problems implemented in various programming languages."
            )
    , itemize
        [ en $ "\\item " ++ year 2015 ++ ", " ++ year 2016 ++ ", " ++ year 2020 ++ ", " ++ year 2021 ++ ", " ++ year 2022 ++ ", " ++ year 2023 ++ "."
        ]
    ]
  where
    year ∷ Int → String
    year n = "\\underline{\\href{https://github.com/japiirainen/aoc-" ++ show n ++ "}{" ++ show n ++ "}}"

domains ∷ Resume
domains =
  paragraph
    [ itemTeX "Programming Domains"
    , en "Experienced and passionate about programming domains such as algorithms, programming language development, web development, and computational methods in mathematics."
    ]

programmingLanguages ∷ Resume
programmingLanguages =
  paragraph
    [ itemTeX "Program Languages"
    , en "\\textbf{Multilingual and adaptable},"
    , en $ "extensive experienced in " ++ very ++ "."
    , en $ "Comfortable working with " ++ somehow ++ " (in no particular order)."
    ]
  where
    somehow = "\\texttt{Rust}, \\texttt{C}, \\texttt{C++}, \\texttt{Java} and \\texttt{Ocaml}"
    very = "\\texttt{TypeScript}, \\texttt{Clojure}, \\texttt{Haskell}, \\texttt{Python} and \\texttt{C\\#}"

webFrameworks ∷ Resume
webFrameworks =
  paragraph
    [ itemTeX "Web Technologies"
    , en $ "Extensive experienced with " ++ very ++ "."
    ]
  where
    very = "\\texttt{React}, \\texttt{Next.js}, \\texttt{FastAPI}, \\texttt{Flask}, \\texttt{dotnet}, and \\texttt{node.js}"

paradigms ∷ Resume
paradigms =
  paragraph
    [ itemTeX "Programming Paradigms"
    , en "In-depth understanding of various programming paradigms, including functional, object-oriented, logic, and array programming. Recognize that different paradigms are suited to different problems, and there are no universal solutions."
    ]

practices ∷ Resume
practices =
  paragraph
    [ itemTeX "Programming Practices"
    , en "Committed to delivering high-quality software with expertise in a range of testing and quality assurance techniques."
    ]

cloud ∷ Resume
cloud =
  paragraph
    [ itemTeX "Cloud Technologies"
    , en "Extensive experience with cloud platforms such as Microsoft Azure and AWS. Skilled in developing and maintaining infrastructure pipelines using Infrastructure as Code (IaC) methodologies."
    ]

wow ∷ Resume
wow =
  paragraph
    [ itemTeX "Ways of Working"
    , en "Passionate about helping teams thrive and achieve project success. Extensive experience facilitating activities such as retrospectives and project kickoffs."
    ]

skills ∷ Resume
skills =
  section "Skills" . pure $
    itemize
      [ wow
      , programmingLanguages
      , webFrameworks
      , cloud
      , paradigms
      , practices
      , domains
      ]
