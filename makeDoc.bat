set SCRIBLE=c:\tools\Racket
set FORMAT=--html
set ODIR=doc\html\
rem если надо документацию в формате md - раскоментировать строки ниже
rem set FORMAT=--markdown
rem set ODIR=doc\
del /Q doc\html\*.*
%SCRIBLE%\scribble.exe %FORMAT% --dest %ODIR% doc\src\README.scrbl 

start doc\html\README.html
