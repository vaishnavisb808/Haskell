@echo off
stack exec -- hlint .
if %ERRORLEVEL% == 1 echo "Implement hlint corrections to continue bulid" & exit
stack exec -- stylish-haskell -i -r .
if %ERRORLEVEL% == 1 echo "Implement stylish-haskell corrections to continue bulid" & exit
stack build --fast -j4
