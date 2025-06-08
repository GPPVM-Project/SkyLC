# =============================================================================
# SkyL Language - Build, Deploy, and Global Setup Script (Debug Mode)
#
# 1. Checks for Administrator privileges and requests them if necessary.
# 2. Verifies if Rust is installed, installs if missing.
# 3. Compiles the project with cargo build.
# 4. Creates the directory structure dist/bin and dist/lib/skyl.
# 5. Copies the skylc.exe binary.
# 6. Copies and renames the stdlib to dist/lib/skyl/stdlib.
# 7. Sets permanent environment variables (SKYL_LIB and PATH).
# =============================================================================

# --- Project Configuration ---
$StdLibSourcePath = "stdlib"  # Folder containing the stdlib (e.g. stdlib/src/*.gpp)
$OutputDir = "dist"
$OutputBinDir = Join-Path $OutputDir "bin"
$OutputLibDir = Join-Path $OutputDir "lib\skyl"
$FinalStdLibPath = Join-Path $OutputLibDir "stdlib"

# 1. Check Administrator privileges
Write-Host "Step 1: Checking for Administrator privileges..."
if (-NOT ([Security.Principal.WindowsPrincipal][Security.Principal.WindowsIdentity]::GetCurrent()).IsInRole([Security.Principal.WindowsBuiltInRole]::Administrator)) {
    Write-Warning "Administrator rights are required to set global environment variables."
    Write-Warning "Requesting elevation..."
    Start-Process powershell.exe -Verb RunAs -ArgumentList "-File `"$($MyInvocation.MyCommand.Path)`""
    exit
}
Write-Host "Running with Administrator privileges." -ForegroundColor Cyan

# Ensure we are in the script directory
Set-Location -Path $PSScriptRoot

# 2. Check if Rust is installed
Write-Host "Step 2: Checking if Rust is installed..."
if (-not (Get-Command "cargo.exe" -ErrorAction SilentlyContinue)) {
    Write-Warning "Rust is not installed. Installing Rust using rustup..."
    Invoke-WebRequest -Uri https://static.rust-lang.org/rustup/init.exe -OutFile "$env:TEMP\rustup-init.exe"
    Start-Process -Wait -FilePath "$env:TEMP\rustup-init.exe" -ArgumentList "-y"
    Remove-Item "$env:TEMP\rustup-init.exe"
    Write-Host "Rust installation completed. Restart this script if any issues occur." -ForegroundColor Cyan
}
else {
    Write-Host "Rust is already installed." -ForegroundColor Green
}

# 3. Compile with cargo
Write-Host "Step 3: Compiling the workspace with 'cargo build'..."
cargo build
if ($LASTEXITCODE -ne 0) {
    Write-Host "Cargo build failed. Aborting." -ForegroundColor Red
    exit 1
}
Write-Host "Compilation finished successfully." -ForegroundColor Cyan

# 4. Prepare directories
Write-Host "Step 4: Preparing output directories in '$OutputDir'..."
if (Test-Path $OutputDir) {
    try {
        Remove-Item -Recurse -Force $OutputDir -ErrorAction Stop
        Write-Host "Previous '$OutputDir' directory removed." -ForegroundColor Gray
    }
    catch {
        Write-Error "Failed to delete '$OutputDir'. Make sure no files are in use."
        exit 1
    }
}
New-Item -ItemType Directory -Force $OutputBinDir | Out-Null
New-Item -ItemType Directory -Force $OutputLibDir | Out-Null
Write-Host "Output directories created." -ForegroundColor Cyan

# 5. Copy the binary
$SourceExe = ".\target\debug\skylc.exe"
if (-not (Test-Path $SourceExe)) {
    Write-Error "Executable '$SourceExe' not found. Build may have failed."
    exit 1
}
Write-Host "Step 5: Copying '$SourceExe' to '$OutputBinDir'..."
Copy-Item -Path $SourceExe -Destination $OutputBinDir
Write-Host "Executable copied." -ForegroundColor Cyan

# 6. Copy the stdlib
Write-Host "Step 6: Copying stdlib from '$StdLibSourcePath' to '$OutputLibDir'..."
if (-not (Test-Path $StdLibSourcePath)) {
    Write-Error "Standard library folder '$StdLibSourcePath' not found."
    exit 1
}
Copy-Item -Path $StdLibSourcePath -Destination $OutputLibDir -Recurse

# Rename stdlib/src → stdlib
if (Test-Path (Join-Path $OutputLibDir "src")) {
    Rename-Item -Path (Join-Path $OutputLibDir "src") -NewName "stdlib"
}
Write-Host "Standard library copied to '$FinalStdLibPath'." -ForegroundColor Cyan

# 7. Set permanent environment variables
Write-Host "Step 7: Setting permanent environment variables..."

$AbsoluteBinPath = (Resolve-Path -Path $OutputBinDir).Path
$AbsoluteLibPath = (Resolve-Path -Path $OutputLibDir).Path

[System.Environment]::SetEnvironmentVariable("SKYL_LIB", $AbsoluteLibPath, [System.EnvironmentVariableTarget]::User)
Write-Host "  SKYL_LIB set to: '$AbsoluteLibPath'" -ForegroundColor Yellow

$CurrentUserPath = [System.Environment]::GetEnvironmentVariable("Path", [System.EnvironmentVariableTarget]::User)
if ($CurrentUserPath -notlike "*$AbsoluteBinPath*") {
    $NewPath = "$AbsoluteBinPath;$CurrentUserPath"
    [System.Environment]::SetEnvironmentVariable("Path", $NewPath, [System.EnvironmentVariableTarget]::User)
    Write-Host "  '$AbsoluteBinPath' added to user PATH." -ForegroundColor Yellow
}
else {
    Write-Host "  '$AbsoluteBinPath' already in user PATH." -ForegroundColor Gray
}

$env:Path = "$AbsoluteBinPath;$($env:Path)"
Write-Host "Environment variables set successfully." -ForegroundColor Cyan

# 8. Finished
Write-Host "`nScript finished successfully!" -ForegroundColor Green
Write-Host "IMPORTANT: Open a NEW terminal to use 'skylc' and SKYL_LIB globally." -ForegroundColor Magenta
