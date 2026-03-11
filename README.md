# cl-lean4-integration

Common Lisp to Lean4 theorem prover integration with **zero external dependencies**.

## Features

- **Expression translation**: CL to Lean4 syntax
- **Type inference**: Automatic type annotations
- **Proof verification**: Send proofs to Lean4 server
- **Tactic generation**: Generate proof tactics
- **Pure Common Lisp**: No CFFI, no external libraries

## Installation

```lisp
(asdf:load-system :cl-lean4-integration)
```

Requires Lean4 installation for verification.

## Quick Start

```lisp
(use-package :cl-lean4-integration)

;; Translate CL function to Lean4
(lean4-translate
  '(defun factorial (n)
     (if (<= n 1)
         1
         (* n (factorial (1- n))))))

;; Verify property
(lean4-verify
  '(forall (n : nat)
     (> (factorial n) 0)))
```

## API Reference

### Translation

- `(lean4-translate form)` - Translate CL form to Lean4
- `(lean4-translate-type type)` - Translate type specification

### Verification

- `(lean4-verify property)` - Verify property in Lean4
- `(lean4-prove goal tactics)` - Apply tactics to prove goal
- `(lean4-check-proof proof)` - Check proof validity

### Server

- `(lean4-start-server)` - Start Lean4 server process
- `(lean4-stop-server)` - Stop server

## Testing

```lisp
(asdf:test-system :cl-lean4-integration)
```

## License

BSD-3-Clause

Copyright (c) 2024-2026 Parkian Company LLC. All rights reserved.
