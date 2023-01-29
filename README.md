Joona Piirainen resume

# Build

To build this resume, install texlive-full and a Haskell toolchain (GHC with Cabal) first.
Then:

```bash
# Generate the TeX files
runghc Main.hs en >> latex/resume.tex
runghc Main.hs elab >> latex/resume-elab.tex

# Build the TeX files
cd latex
make
cp *.pdf ..
cd ..
```

# Download

+ [English one-page version (pdf)](./resume.pdf)
