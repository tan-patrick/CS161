@echo off
rem MS DOS Batch file to run SamIam
rem by Keith Cascio

rem Explanation of command-line flags:
rem
rem -Xruncalljvmti: Run the virtual machine profiler for Java version >= 5 (calljvmti.dll).
rem
rem -Xms8m: Specify the initial size, in bytes, of the memory allocation pool = 8 Megs.
rem
rem -Xmx2048m: Specify the maximum size, in bytes, of the memory allocation pool = 2048 Megs.

set VMARGS=-Xruncalljvmti -Xms8m -Xmx2048m -classpath samiam.jar;inflib.jar edu.ucla.belief.ui.UI

call :which java.exe
%EXECCMD% %VMARGS% -launchcommand "%EXECCMD% %VMARGS% %*" -launchscript %0 %*
goto :EOF

:which
set EXECCMD=%~$PATH:1
