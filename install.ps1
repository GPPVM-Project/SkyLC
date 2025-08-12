# =============================================================================
# SkyL Language - Build, Deploy, and Global Setup Script (Debug Mode)
# =============================================================================

$ErrorActionPreference = "Stop"

# --- Project Configuration ---
$StdLibSourcePath = "stdlib"
$OutputDir = "dist"
$OutputBinDir = Join-Path $OutputDir "bin"
$OutputLibDir = Join-Path $OutputDir "lib\skyl"
$FinalStdLibPath = Join-Path $OutputLibDir "stdlib"

# 1. Check Administrator privileges
Write-Host "Step 1: Checking for Administrator privileges..."
if (-NOT ([Security.Principal.WindowsPrincipal][Security.Principal.WindowsIdentity]::GetCurrent()).IsInRole([Security.Principal.WindowsBuiltInRole]::Administrator)) {
    Write-Warning "Administrator rights are required. Requesting elevation..."
    Start-Process powershell.exe -Verb RunAs -ArgumentList "-ExecutionPolicy Bypass -File `"$($MyInvocation.MyCommand.Path)`""
    exit
}
Write-Host "Running with Administrator privileges." -ForegroundColor Cyan

Set-Location -Path $PSScriptRoot

# 2. Install Visual Studio Build Tools (C++ workload) if not found
Write-Host "Step 2: Checking for Visual Studio C++ Build Tools..."
# Check for presence of link.exe (common linker for MSVC toolchain)
if (-not (Get-Command link.exe -ErrorAction SilentlyContinue)) {
    Write-Warning "Visual Studio C++ Build Tools not found. Installing..."

    $VSInstaller = "$env:TEMP\vs_buildtools.exe"
    if (Test-Path $VSInstaller) {
        Remove-Item $VSInstaller -Force
    }

    Invoke-WebRequest "https://aka.ms/vs/17/release/vs_BuildTools.exe" -OutFile $VSInstaller -UseBasicParsing

    $info = Get-Item $VSInstaller
    Write-Host "Downloaded $($info.Name), size: $([math]::Round($info.Length / 1MB, 2)) MB"

    if ($info.Length -lt 50000000) {  # Se arquivo < 50MB, suspeito de download falho
        Write-Error "Download incompleto ou corrompido. Por favor, tente novamente."
        Read-Host "Press Enter to exit"
        exit 1
    }

    Start-Process -FilePath $VSInstaller -ArgumentList `
        "--quiet --wait --norestart --nocache --add Microsoft.VisualStudio.Workload.VCTools" `
        -Wait -NoNewWindow

    Remove-Item $VSInstaller

    Write-Host "Visual Studio C++ Build Tools instalado com sucesso." -ForegroundColor Green
} else {
    Write-Host "Visual Studio C++ Build Tools já instalado." -ForegroundColor Green
}

# 3. Check if Rust is installed
Write-Host "Step 3: Checking if Rust is installed..."
$CargoPath = Join-Path $env:USERPROFILE ".cargo\bin\cargo.exe"

if (-not (Test-Path $CargoPath)) {
    Write-Warning "Rust is not installed. Installing Rust..."
    $RustupPath = "$env:TEMP\rustup-init.exe"
    Invoke-WebRequest https://win.rustup.rs/x86_64 -OutFile $RustupPath -UseBasicParsing
    Start-Process -Wait -FilePath $RustupPath -ArgumentList "-y"
    Remove-Item $RustupPath

    if (-not (Test-Path $CargoPath)) {
        Write-Error "Rust installation failed. Please install manually from https://rustup.rs/"
        Read-Host "Press Enter to exit"
        exit 1
    }
    Write-Host "Rust installation completed." -ForegroundColor Green
} else {
    Write-Host "Rust is already installed." -ForegroundColor Green
}

# 4. Compile with cargo
Write-Host "Step 4: Compiling with cargo..."
& $CargoPath build --release
if ($LASTEXITCODE -ne 0) {
    Write-Error "Cargo build failed."
    Read-Host "Press Enter to exit"
    exit 1
}
Write-Host "Compilation successful." -ForegroundColor Cyan

# 5. Prepare output directories
Write-Host "Step 5: Preparing output directories..."
if (Test-Path $OutputDir) {
    Remove-Item -Recurse -Force $OutputDir
}
New-Item -ItemType Directory -Force $OutputBinDir | Out-Null
New-Item -ItemType Directory -Force $OutputLibDir | Out-Null

# 6. Copy the binary
$SourceExe = ".\target\release\skylc.exe"
if (-not (Test-Path $SourceExe)) {
    Write-Error "Executable '$SourceExe' not found."
    Read-Host "Press Enter to exit"
    exit 1
}
Copy-Item -Path $SourceExe -Destination $OutputBinDir

# 7. Copy stdlib
if (-not (Test-Path $StdLibSourcePath)) {
    Write-Error "Stdlib folder '$StdLibSourcePath' not found."
    Read-Host "Press Enter to exit"
    exit 1
}
Copy-Item -Path $StdLibSourcePath -Destination $OutputLibDir -Recurse
if (Test-Path (Join-Path $OutputLibDir "src")) {
    Rename-Item -Path (Join-Path $OutputLibDir "src") -NewName "stdlib"
}

# 8. Set environment variables
$AbsoluteBinPath = (Resolve-Path -Path $OutputBinDir).Path
$AbsoluteLibPath = (Resolve-Path -Path $OutputLibDir).Path
[System.Environment]::SetEnvironmentVariable("SKYL_LIB", $AbsoluteLibPath, [System.EnvironmentVariableTarget]::User)

$CurrentUserPath = [System.Environment]::GetEnvironmentVariable("Path", [System.EnvironmentVariableTarget]::User)
if ($CurrentUserPath -notlike "*$AbsoluteBinPath*") {
    [System.Environment]::SetEnvironmentVariable("Path", "$AbsoluteBinPath;$CurrentUserPath", [System.EnvironmentVariableTarget]::User)
}

Write-Host "`nScript finished successfully!" -ForegroundColor Green
Write-Host "IMPORTANT: Open a NEW terminal to use 'skylc' globally." -ForegroundColor Magenta
Read-Host "Press Enter to exit"
