module Resume (resume) where

import ResumeData
import TeX

resume ∷ Resume
resume =
  paragraph
    [ pure "% !TEX program = xelatex"
    , pure "% This file is generated, don't manually edit!"
    , paragraph
        [ pure "\\documentclass{resume}"
        , pure "\\usepackage{linespacing_fix}"
        ]
    , -- begin document
      pure "\\begin{document}"
    , tex "name" "Joona Piirainen"
    , basicInfo
    , section
        "Job Experience"
        [ adalyon
        , reaktor
        , puheet
        ]
    , skills
    , section
        "Projects and Contributions"
        [ fp
        , vl
        , aoc
        ]
    , section
        "Miscellaneous"
        [ itemize
            [ en "\\item As a hobby I like to do rock climbing, solve programming puzzles and study mathematics. Lately I've been studying Algebraic Geometry and specifically it's computational aspects."
            ]
        ]
    , -- end document
      pure "\\end{document}"
    ]
