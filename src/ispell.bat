@echo off
echo @(#) Poor Man's Ispell Version 3.2.06 08/01/01

if "%1%"=="-vv" (exit /B 0)

:nextline
  set /p word=
  if "%eof%" == "1" (exit /B 0)
  if "%word:~0,1%" == "^" (echo *)
  echo.
goto nextline
