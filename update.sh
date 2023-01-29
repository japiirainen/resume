#!/bin/bash

echo "======generating LaTeX========"

make -C latex clean-tex
cabal run resume --verbose=0 -- en >> latex/resume.tex

echo "=======building LaTeX========="

make -C latex >> latex/make.log
mv latex/*.pdf .
make -C latex clean-tex

echo "====removing useless files===="

rm latex/*.log
rm latex/*.aux
rm latex/*.out
rm latex/*.fls
rm latex/*.bcf
rm latex/*.xml
