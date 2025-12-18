@echo off
setlocal EnableExtensions EnableDelayedExpansion

REM ----------------------------------------------------------------------------- 
REM Config
REM -----------------------------------------------------------------------------
set "R_EXE=R"
REM oppure:
REM set "R_EXE=C:\Program Files\R\R-4.4.2\bin\R.exe"

set "BASE_DIR=D:\Google Drive\LUCAS 2026"
set "SCRIPT_DIR=%BASE_DIR%\SCRIPTS"
set "LOG_DIR=%BASE_DIR%\logs"

if not exist "%LOG_DIR%" mkdir "%LOG_DIR%"

set "MASTER_LOG=%LOG_DIR%\pipeline.log"

echo =============================================================== > "%MASTER_LOG%"
echo LUCAS27 Sample Selection - START %DATE% %TIME% >> "%MASTER_LOG%"
echo =============================================================== >> "%MASTER_LOG%"

REM ----------------------------------------------------------------------------- 
REM Run function
REM -----------------------------------------------------------------------------
goto :main

:run
set "STEP=%~1"
set "SCRIPT=%~2"
set "LOGFILE=%LOG_DIR%\%~3.Rout"

echo. >> "%MASTER_LOG%"
echo [%DATE% %TIME%] START  %STEP% >> "%MASTER_LOG%"
echo Script: %SCRIPT% >> "%MASTER_LOG%"
echo Log: %LOGFILE% >> "%MASTER_LOG%"

echo.
echo ===============================================================================
echo [RUN] %STEP%
echo -------------------------------------------------------------------------------

"%R_EXE%" CMD BATCH --vanilla "%SCRIPT%" "%LOGFILE%"
set "RC=%ERRORLEVEL%"

if not "%RC%"=="0" (
    echo [%DATE% %TIME%] ERROR %STEP% (exit code %RC%) >> "%MASTER_LOG%"
    echo Pipeline stopped. >> "%MASTER_LOG%"
    echo. >> "%MASTER_LOG%"

    echo !!! ERROR in step: %STEP%
    echo See log: %LOGFILE%
    exit /b %RC%
)

echo [%DATE% %TIME%] OK    %STEP% >> "%MASTER_LOG%"
exit /b 0

REM ----------------------------------------------------------------------------- 
REM Main pipeline
REM -----------------------------------------------------------------------------
:main

REM Soil
call :run "Soil" "%SCRIPT_DIR%\1.1.Soil_sample.R" "1.1.Soil_sample"

REM Grassland
call :run "Grassland component 1" "%SCRIPT_DIR%\2.1.Select_grassland_component1.R" "2.1.Grassland_c1"
call :run "Grassland component 2" "%SCRIPT_DIR%\2.2.Select_grassland_component2.R" "2.2.Grassland_c2"
call :run "Grassland join"        "%SCRIPT_DIR%\2.3.Join_Grassland_components.R"  "2.3.Grassland_join"

REM Landscape Features
call :run "LF prepare"        "%SCRIPT_DIR%\3.1.Prepare_LF.R"          "3.1.LF_prepare"
call :run "LF process"        "%SCRIPT_DIR%\3.2.LF_process.R"          "3.2.LF_process"
call :run "LF allocate"       "%SCRIPT_DIR%\3.3.LF_alloca.R"           "3.3.LF_alloca"
call :run "LF select panel"   "%SCRIPT_DIR%\3.4.Select_LF_panel.R"     "3.4.LF_panel"
call :run "LF select nonpanel""%SCRIPT_DIR%\3.5.Select_LF_nonpanel.R"  "3.5.LF_nonpanel"
call :run "LF join"           "%SCRIPT_DIR%\3.6.Join_LF_components.R"  "3.6.LF_join"

REM Copernicus
call :run "Copernicus prepare" "%SCRIPT_DIR%\4.1.Prepare_Copernicus.R"  "4.1.Copernicus_prepare"
call :run "Copernicus select"  "%SCRIPT_DIR%\4.2.Select_Copernicus.R"   "4.2.Copernicus_select"
call :run "Copernicus total"   "%SCRIPT_DIR%\4.3.Copernicus_Total.R"    "4.3.Copernicus_total"

REM Final
call :run "Final LUCAS27 sample" "%SCRIPT_DIR%\5.Final_LUCAS27_sample.R" "5.Final_sample"

echo. >> "%MASTER_LOG%"
echo =============================================================== >> "%MASTER_LOG%"
echo LUCAS27 Sample Selection - END %DATE% %TIME% >> "%MASTER_LOG%"
echo =============================================================== >> "%MASTER_LOG%"

echo.
echo ===============================================================================
echo PIPELINE COMPLETED SUCCESSFULLY
echo Logs in: %LOG_DIR%
echo ===============================================================================
exit /b 0
