#!/usr/bin/env bash
set -e

# =============================================================================
# SkyL Language - Build, Deploy, and Global Setup Script (Linux - Debug Mode)
#
# 1. Verifica privilégios de root quando necessário.
# 2. Verifica se Rust está instalado, instala se estiver ausente.
# 3. Compila o projeto com cargo build --release.
# 4. Cria diretórios dist/bin e dist/lib/skyl.
# 5. Copia o binário skylc.
# 6. Copia e renomeia a stdlib para dist/lib/skyl/stdlib.
# 7. Configura variáveis de ambiente permanentes (SKYL_LIB e PATH).
# =============================================================================

STDLIB_SOURCE_PATH="stdlib"
OUTPUT_DIR="dist"
OUTPUT_BIN_DIR="$OUTPUT_DIR/bin"
OUTPUT_LIB_DIR="$OUTPUT_DIR/lib/skyl"
FINAL_STDLIB_PATH="$OUTPUT_LIB_DIR/stdlib"

# === 1. Verifica privilégios de root quando necessário ===
if [ "$EUID" -ne 0 ]; then
    echo "[1/7] É recomendado executar com privilégios de root para configurar variáveis de ambiente globais."
    echo "       Você pode ser solicitado a digitar sua senha."
    sudo "$0" "$@"
    exit $?
fi

# Garante que o script rode no diretório onde está localizado
cd "$(dirname "$0")"

# === 2. Verifica se Rust está instalado ===
echo "[2/7] Verificando instalação do Rust..."
if ! command -v cargo >/dev/null 2>&1; then
    echo "Rust não encontrado. Instalando via rustup..."
    curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh -s -- -y
    source "$HOME/.cargo/env"
    echo "Rust instalado com sucesso."
else
    echo "Rust já está instalado."
fi

# === 3. Compila com cargo ===
echo "[3/7] Compilando o projeto (modo release)..."
cargo build --release
echo "Compilação concluída."

# === 4. Prepara diretórios ===
echo "[4/7] Preparando diretórios de saída..."
rm -rf "$OUTPUT_DIR"
mkdir -p "$OUTPUT_BIN_DIR" "$OUTPUT_LIB_DIR"

# === 5. Copia o binário ===
SOURCE_EXE="./target/release/skylc"
if [ ! -f "$SOURCE_EXE" ]; then
    echo "Erro: Binário '$SOURCE_EXE' não encontrado."
    exit 1
fi
cp "$SOURCE_EXE" "$OUTPUT_BIN_DIR/"
echo "Binário copiado para $OUTPUT_BIN_DIR."

# === 6. Copia a stdlib ===
echo "[6/7] Copiando stdlib..."
if [ ! -d "$STDLIB_SOURCE_PATH" ]; then
    echo "Erro: Pasta da stdlib '$STDLIB_SOURCE_PATH' não encontrada."
    exit 1
fi
cp -r "$STDLIB_SOURCE_PATH" "$OUTPUT_LIB_DIR/"
if [ -d "$OUTPUT_LIB_DIR/src" ]; then
    mv "$OUTPUT_LIB_DIR/src" "$FINAL_STDLIB_PATH"
fi
echo "Stdlib copiada para $FINAL_STDLIB_PATH."

# === 7. Configura variáveis de ambiente ===
echo "[7/7] Configurando variáveis de ambiente..."
ABS_BIN_PATH="$(realpath "$OUTPUT_BIN_DIR")"
ABS_LIB_PATH="$(realpath "$OUTPUT_LIB_DIR")"

# Adiciona SKYL_LIB ao ~/.bashrc
if ! grep -q "export SKYL_LIB=" "$HOME/.bashrc"; then
    echo "export SKYL_LIB=\"$ABS_LIB_PATH\"" >> "$HOME/.bashrc"
    echo "SKYL_LIB configurado em $ABS_LIB_PATH"
else
    echo "SKYL_LIB já configurado no ~/.bashrc"
fi

# Adiciona binário ao PATH no ~/.bashrc
if ! grep -q "$ABS_BIN_PATH" "$HOME/.bashrc"; then
    echo "export PATH=\"$ABS_BIN_PATH:\$PATH\"" >> "$HOME/.bashrc"
    echo "PATH atualizado com $ABS_BIN_PATH"
else
    echo "PATH já contém $ABS_BIN_PATH"
fi

echo -e "\n✅ Instalação concluída!"
echo "Abra um novo terminal para usar 'skylc' e a variável SKYL_LIB."
