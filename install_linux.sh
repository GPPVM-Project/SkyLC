#!/usr/bin/env bash
set -e

# =============================================================================
# SkyL Language - Build, Deploy, and Global Setup Script (Linux - Debug Mode)
#
# 1. Verifica se Rust está instalado, instala se estiver ausente.
# 2. Compila o projeto com cargo build --release.
# 3. Cria diretórios dist/bin e dist/lib/skyl.
# 4. Copia o binário skylc.
# 5. Copia e renomeia a stdlib para dist/lib/skyl/stdlib.
# 6. Configura variáveis de ambiente permanentes (SKYL_LIB e PATH).
# =============================================================================

STDLIB_SOURCE_PATH="stdlib"
OUTPUT_DIR="dist"
OUTPUT_BIN_DIR="$OUTPUT_DIR/bin"
OUTPUT_LIB_DIR="$OUTPUT_DIR/lib/skyl"
FINAL_STDLIB_PATH="$OUTPUT_LIB_DIR/stdlib"

# Garante que o script rode no diretório onde está localizado
cd "$(dirname "$0")"

# === 1. Verifica se Rust está instalado ===
echo "[1/6] Verificando instalação do Rust..."
if ! command -v cargo >/dev/null 2>&1; then
    echo "Rust não encontrado. Instalando via rustup..."
    curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh -s -- -y
    # Carrega o ambiente do Rust para o shell atual
    source "$HOME/.cargo/env"
    export PATH="$HOME/.cargo/bin:$PATH"
    echo "Rust instalado com sucesso."
else
    echo "Rust já está instalado."
fi

# === 2. Compila com cargo ===
echo "[2/6] Compilando o projeto (modo release)..."
cargo build --release
echo "Compilação concluída."

# === 3. Prepara diretórios ===
echo "[3/6] Preparando diretórios de saída..."
rm -rf "$OUTPUT_DIR"
mkdir -p "$OUTPUT_BIN_DIR" "$OUTPUT_LIB_DIR"

# === 4. Copia o binário ===
SOURCE_EXE="./target/release/skylc"
if [ ! -f "$SOURCE_EXE" ]; then
    echo "Erro: Binário '$SOURCE_EXE' não encontrado."
    exit 1
fi
cp "$SOURCE_EXE" "$OUTPUT_BIN_DIR/"
echo "Binário copiado para $OUTPUT_BIN_DIR."

# === 5. Copia a stdlib ===
echo "[5/6] Copiando stdlib..."
if [ ! -d "$STDLIB_SOURCE_PATH" ]; then
    echo "Erro: Pasta da stdlib '$STDLIB_SOURCE_PATH' não encontrada."
    exit 1
fi
cp -r "$STDLIB_SOURCE_PATH" "$OUTPUT_LIB_DIR/"
if [ -d "$OUTPUT_LIB_DIR/src" ]; then
    mv "$OUTPUT_LIB_DIR/src" "$FINAL_STDLIB_PATH"
fi
echo "Stdlib copiada para $FINAL_STDLIB_PATH."

# === 6. Configura variáveis de ambiente ===
echo "[6/6] Configurando variáveis de ambiente..."
ABS_BIN_PATH="$(realpath "$OUTPUT_BIN_DIR")"
ABS_LIB_PATH="$(realpath "$OUTPUT_LIB_DIR")"

# Adiciona SKYL_LIB ao ~/.bashrc se ainda não estiver presente
if ! grep -q "export SKYL_LIB=" "$HOME/.bashrc"; then
    echo "export SKYL_LIB=\"$ABS_LIB_PATH\"" >> "$HOME/.bashrc"
    echo "SKYL_LIB configurado em $ABS_LIB_PATH"
else
    echo "SKYL_LIB já configurado no ~/.bashrc"
fi

# Adiciona binário ao PATH no ~/.bashrc se ainda não estiver presente
if ! grep -q "$ABS_BIN_PATH" "$HOME/.bashrc"; then
    echo "export PATH=\"$ABS_BIN_PATH:\$PATH\"" >> "$HOME/.bashrc"
    echo "PATH atualizado com $ABS_BIN_PATH"
else
    echo "PATH já contém $ABS_BIN_PATH"
fi

echo -e "\n✅ Instalação concluída!"
echo "Abra um novo terminal para usar 'skylc' e a variável SKYL_LIB."
