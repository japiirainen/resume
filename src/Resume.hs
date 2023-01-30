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
        [ reaktor
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
        [ let
            codewars = href "https://www.codewars.com/" "CodeWars"
            leetcode = href "https://www.leetcode.com/" "LeetCode"
           in
            itemize
              [ en $ "\\item I love solving coding problems, I'm quite active in " ++ leetcode ++ " and " ++ codewars
              , en "\\item I never studied math in school, but later in life I've become quite interested in it. Lately I've been studying \\texttt{Number Theory} and \\texttt{Category Theory}."
              , en "\\item I'm a former professional athlete. I know what it takes to be great at something and am willing to put in the work to get there."
              ]
        ]
    , -- end document
      pure "\\end{document}"
    ]
