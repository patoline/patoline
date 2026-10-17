@echo off
REM push.bat - commit and push the current working tree to origin.
REM
REM Usage:
REM   push.bat                          commit with the default message
REM   push.bat "my message"             commit with a custom message
REM   push.bat "my message" --amend     amend the previous commit instead
REM   push.bat --dry-run                show what would happen, change nothing
REM   push.bat --help                   show this help
REM
REM Stages everything that is not git-ignored, commits it with the given
REM message, and pushes the current branch to its remote.
REM
REM Proxy: a VPN on this machine is expected on http://127.0.0.1:7897.
REM Set PUSH_PROXY to point somewhere else, or PUSH_PROXY=none to skip.

setlocal enabledelayedexpansion

cd /d "%~dp0"

set "DRY_RUN=0"
set "AMEND="
set "MSG="

:parseArgs
if "%~1"=="" goto :doneArgs
if /i "%~1"=="--dry-run" ( set "DRY_RUN=1" & shift & goto :parseArgs )
if /i "%~1"=="--amend"   ( set "AMEND=--amend" & shift & goto :parseArgs )
if /i "%~1"=="--help"    ( goto :showHelp )
set "MSG=%~1"
shift
goto :parseArgs
:doneArgs

where git >nul 2>&1
if errorlevel 1 (
  echo [ERROR] git was not found on PATH.
  exit /b 1
)

if not exist ".git" (
  echo [ERROR] %CD% is not a git repository.
  exit /b 1
)

REM Pass the local VPN proxy to git through its own config, which is the only
REM way git honours a proxy on Windows.  The system-wide proxy setting is not
REM used by git, so without this a push silently hangs until the TCP timeout.
if not defined PUSH_PROXY set "PUSH_PROXY=http://127.0.0.1:7897"
if /i "%PUSH_PROXY%"=="none" goto :proxyDone

set "GIT_PROXY_ARGS=-c http.proxy=%PUSH_PROXY% -c https.proxy=%PUSH_PROXY%"
echo [NOTE] using proxy %PUSH_PROXY% for git.

:proxyDone

for /f "delims=" %%b in ('git rev-parse --abbrev-ref HEAD 2^>nul') do set "BRANCH=%%b"
if not defined BRANCH (
  echo [ERROR] could not determine the current branch.
  exit /b 1
)
if "%BRANCH%"=="HEAD" (
  echo [ERROR] detached HEAD - check out a branch first.
  exit /b 1
)

set "REMOTE="
for /f "delims=" %%r in ('git config "branch.%BRANCH%.remote" 2^>nul') do set "REMOTE=%%r"
if not defined REMOTE set "REMOTE=origin"

REM Verify the remote really exists (git remote get-url fails for unknown names).
git remote get-url "%REMOTE%" >nul 2>&1
if errorlevel 1 (
  echo [ERROR] remote "%REMOTE%" does not exist.
  exit /b 1
)

REM git needs an identity; fall back to a local default when none is set.
git config user.name  >nul 2>&1
if errorlevel 1 (
  echo [NOTE] user.name is unset; using %USERNAME% for this commit.
  set "GIT_AUTHOR_NAME=%USERNAME%"
  set "GIT_COMMITTER_NAME=%USERNAME%"
)
git config user.email >nul 2>&1
if errorlevel 1 (
  echo [NOTE] user.email is unset; using %USERNAME%@localhost for this commit.
  set "GIT_AUTHOR_EMAIL=%USERNAME%@localhost"
  set "GIT_COMMITTER_EMAIL=%USERNAME%@localhost"
)

if not defined MSG set "MSG=chore: sync working tree (%BRANCH%)"

echo ==^> staging changes
if "%DRY_RUN%"=="1" (
  echo [dry-run] git add -A
  goto :checkPending
)
git add -A
if errorlevel 1 (
  echo [ERROR] git add failed.
  exit /b 1
)

:checkPending
REM "git status --porcelain" prints one line per pending change (staged,
REM modified or untracked).  An empty result means the index already matches
REM HEAD and there is nothing new to record.
set "PENDING="
for /f "delims=" %%s in ('git status --porcelain') do set "PENDING=%%s"
if not defined PENDING (
  echo ==^> nothing to commit, working tree is clean.
  goto :doPush
)

echo ==^> committing: %MSG%
if "%DRY_RUN%"=="1" (
  echo [dry-run] git commit %AMEND% -m "%MSG%"
  goto :doPush
)
git commit %AMEND% -m "%MSG%"
if errorlevel 1 (
  echo [ERROR] git commit failed.
  exit /b 1
)

:doPush
echo ==^> pushing to %REMOTE% %BRANCH%
if "%DRY_RUN%"=="1" (
  echo [dry-run] git push %REMOTE% %BRANCH%
  goto :finish
)
git %GIT_PROXY_ARGS% push "%REMOTE%" "%BRANCH%"
if errorlevel 1 (
  echo [ERROR] git push failed.
  exit /b 1
)

:finish
echo ==^> done.
exit /b 0

:showHelp
echo push.bat - commit and push the current working tree to origin.
echo.
echo Usage:
echo   push.bat                          commit with the default message
echo   push.bat "my message"             commit with a custom message
echo   push.bat "my message" --amend     amend the previous commit instead
echo   push.bat --dry-run                show what would happen, change nothing
echo   push.bat --help                   show this help
echo.
echo Proxy: git is given http.proxy=http://127.0.0.1:7897 by default.
echo Override with PUSH_PROXY, or disable with PUSH_PROXY=none.
exit /b 0
