# Specification: Riscy Language

## 1. Core Principles
* **Target:** RISC-V 32AIM Softcore (FPGA).
* **Philosophy:** Maximize Safety/Performance to Implementation Logic (Sanity) ratio.
* **Memory Model:** Manual management with goodies (e.g. defer, strict alignment, explicit volatility).

## 2. Type System & Semantics

### 2.1 Volatile vs. Normal Pointers
To ensure hardware registers are not optimized away, the type system distinguishes between `*T` (Normal) and `~T` (Volatile).
* **`~T` (Volatile):** Loads/Stores emit distinct IR instructions preventing reordering or caching.
* **Casting Rules:**
    * `*T` -> `~T`: **Allowed Implicitly.** (Restricting a set of types is safe sematically).
    * `~T` -> `*T`: **Forbidden.** Casting volatile to normal implies the value is stable, which is physically false for hardware registers.

### 2.2 Slices (Fat Pointers)
Slices are first-class primitives consisting of a pointer and a length.
* **Syntax:** `[]T`
* **Construction:** `arr[start..end]`
* **Semantics:** Range is **exclusive** of the end index.
* **Safety:**
    * Accessing `slice[i]` emits a bounds check: `if (i >= slice.len) panic()`.
    * This check is mandatory unless explicitly disabled via `@stopBoundsChecking()` within a code block (used for critical paths).

### 2.3 Variable Scoping
* **Shadowing:** Strictly **Forbidden**. A variable declared in an inner scope cannot have the same name as a variable in an outer scope.
* **Declaration:** All variables must be initialized with a value, even if it's `undefined`.
    * `var x: u32 = undefined;` (Explicitly uninitialized).

## 3. Most Important Control Flow Changes From C

### 3.1 Assignment as Statement
Assignment (`a = b`) is **not** an expression. It returns no value.
* **Why:** Prevents `if (a = b)` bugs.
* **Result:** `while ((c = getchar()) != EOF)` is illegal. Must be:
    ```zig
    while (true) {
        c = getchar();
        if (c == EOF) break;
    }
    ```

### 3.2 Iterator Loops
C-style `for` loops are removed to simplify compiler analysis and prevent bounds errors.
* **Syntax:** `for (iterable) |capture, index| { ... }`
* **Behavior:**
    * Loop bounds are calculated *once* at start (unless iterable is infinite).
    * `index` is strictly managed by the compiler (cannot be modified by user).

## 4. Hardware Interface (RISC-V Backend)

### 4.1 Helpful ISA Mapping Table
| Language Construct | RISC-V ASM Strategy | Notes |
|-------------------|---------------------|-------|
| `ptr.*`           | `LW` / `SW`         | Standard access |
| `ptr.~`           | `LW` / `SW` (vol)   | Backend marks as side-effect |
| `slice[i]`        | `BGEU index, len, trap` | Unsigned compare handles indexes less than 0 as well |
| `switch(x){1..5}` | `BLT`, `BGE` or Table | Optimizes to BST or Jump Table |
| `fn.naked`        | No stack handling by the compiler | User controls `ret` |
| `fn.interrupt`    | Context Save (`mret`) | Saves `ra`, `t0-t6`, `a0-a7`, etc. |

## 5. Compiler Implementation Notes

### 5.1 Defer Implementation
`defer` is implemented as an AST transformation.
1. Parse function body.
2. Maintain a LIFO stack of `defer` nodes.
3. Upon encountering `return` or block exit:
    * Pop stack.
    * Inject deferred nodes before the jump.
