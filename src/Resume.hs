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
            [ en "\\item As a hobby, I enjoy rock climbing, solving programming puzzles, and studying mathematics. Recently, I have been focusing on Algebraic Geometry and its computational aspects."
            ]
        ]
    , pure "\\end{document}"
    ]
