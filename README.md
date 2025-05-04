# GPPVM

**GPPVM** (General Purpose Programming Virtual Machine) is a modern programming language and stack-based virtual machine inspired by Java and C#, designed with strong static semantics and efficient bytecode execution.

> “Compile with confidence, execute with elegance.”

---

## ✨ Overview

**GPPVM** combines an expressive language with a performant and extensible virtual machine. It is modular by design, enabling the compiler to be extended or customized through a dynamic plugin system.

### Key Features

- 🔍 **Full semantic analysis** before code generation
- 💡 **Type inference** with implicit coercion based on archetypes
- 🧠 **Robust type system** supporting hierarchical archetypes  
  *(e.g., `int → number → object`)*
- ⚙️ **Bytecode execution** on a stack-based VM
- 🧩 **Plugin-based compiler architecture**
- 🚀 Focus on performance, clean IR, and readable bytecode

---

## 🔧 Modular Architecture

The compiler and VM are composed of well-defined, interchangeable stages. Each stage is implemented as a plugin that can be required, replaced, or extended:

- **Lexer** (required)
- **Parser** (required)
- **Semantic Analyzer** (required)
- **Intermediate Representation Generator (IR)** (required)
- **Bytecode Emitter** (required)
- **Bytecode Decompiler** (optional)
- **Optimizer 1** (optional)
- **Optimizer 2** (optional)
- **Optimizer 3** (optional)
- **Virtual Machine (runtime)**

Project configuration determines which plugins are active for each compilation step.

---
👉 See the full [0.1.0 TODO progress](TODO.md)

---

## 📦 Getting Started

### Requirements

- [Rust](https://www.rust-lang.org/) (version ≥ 1.75)
- Cargo (comes with Rust)

### Building the Compiler

```bash
cargo build --release
```