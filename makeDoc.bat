set SCRIBLE=d:\tools\Racket
del /Q doc\html\*.*
%SCRIBLE%\scribble.exe  --dest doc\html\ doc\src\README.scrbl 
start doc\html\README.html
