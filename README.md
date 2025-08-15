# GPPVM

**GPPVM** (General Purpose Parallel Virtual Machine) is a modern programming language and stack-based virtual machine inspired by Python and Rust, designed with strong static semantics and efficient bytecode execution.

> “Compile with confidence, execute with elegance.”

---

## ✨ Overview

**GPPVM** combines an expressive language with a performant and extensible virtual machine.

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

The compiler and VM are composed of well-defined, interchangeable stages:

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

Windows
```bash
./install.ps1
```