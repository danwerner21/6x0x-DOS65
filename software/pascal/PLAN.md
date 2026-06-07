# Pascal Compiler & P-Code Runtime for DOS/65 (PC6502)

## Overview

This project implements a two-component Pascal toolchain for the PC6502 running DOS/65:

1. **PASCAL.COM** — Pascal compiler: reads `.PAS` source → emits `.PCD` p-code bytecode
2. **PRUN.COM** — P-code runtime engine: reads `.PCD` → interprets/executes on the 6502

Both components are written in 6502 assembly (ca65), built with ld65, and distributed as standard DOS/65 `.COM` files loaded at `$0800`.

The design is inspired by the UCSD Pascal p-System (1978), which successfully ran Pascal on 6502 systems (Apple II, etc.) with 16–64 KB RAM. We implement a **simplified subset** suitable for the DOS/65 memory model and the educational/retro context.

---

## Architecture Decision: Two-Pass Compiler + Interpreter

```
[source.pas] → PASCAL.COM → [source.pcd]  (p-code bytecode file)
[source.pcd] → PRUN.COM  → execution       (p-code interpreter)
```

**Why two separate programs?**
- DOS/65 `.COM` files have a limited address space ($0800–$B7DF, ~44 KB usable).
- The compiler is large; the runtime is modest. Separating them lets each fit comfortably.
- Users can distribute `.PCD` files and run them without recompiling.
- Mirrors the original UCSD p-System model.

**Alternative considered:** Native-code compiler (Pascal → 6502 machine code). Rejected because it is far harder to implement correctly, produces larger programs, and offers no portability advantage on a single-target system.

---

## Pascal Language Subset (Phase 1)

Implement standard Pascal sufficient for real programs:

### Data Types
- `INTEGER` (16-bit signed, −32768..32767)
- `CHAR` (8-bit ASCII)
- `BOOLEAN` (`TRUE`/`FALSE`)
- `STRING` (length-prefixed, max 255 chars)
- Arrays: `ARRAY [lo..hi] OF basetype`
- Records: `RECORD … END`
- Pointer types (heap allocation via `NEW`/`DISPOSE`)

### Expressions
- Integer: `+`, `-`, `*`, `DIV`, `MOD`
- `REAL`: `+`, `-`, `*`, `/` with automatic `INTEGER`/`REAL` coercions
- Boolean: `AND`, `OR`, `NOT`
- Comparison: `=`, `<>`, `<`, `>`, `<=`, `>=`
- String concatenation: `+` (on strings)
- Parenthesised sub-expressions

### Statements
- Assignment `:=`
- `IF … THEN … ELSE …`
- `WHILE … DO …`
- `REPEAT … UNTIL …`
- `FOR i := lo TO/DOWNTO hi DO …`
- `CASE … OF … END`
- `BEGIN … END` compound
- Procedure/function call
- `WRITE`, `WRITELN`, `READ`, `READLN` (standard I/O)

### Program Structure
- `PROGRAM name;`
- `VAR` declarations (global and local)
- `CONST` declarations
- `TYPE` declarations
- `PROCEDURE` and `FUNCTION` (nested, with lexical scoping)
- `BEGIN … END.` main body

### Deferred additions
- Typed file I/O (`FILE OF X`, random-access records)
---

## P-Code Instruction Set

The p-machine is a **stack-based virtual machine**. All operands live on a value stack; instructions pop inputs and push results.

### Machine Registers (implemented in zero page)
```
SP   — stack pointer (2 bytes, points into stack array)
IPC  — interpreter program counter (2 bytes, into p-code buffer)
MP   — mark/frame pointer for current activation record (2 bytes)
NP   — heap top pointer (2 bytes, grows downward from $B7DF)
BASE — globals base pointer (2 bytes, = $0800 + header)
```

### Stack Layout per Activation Record
```
[MP+0..1] — saved MP (dynamic link)
[MP+2..3] — saved IPC (return address)
[MP+4..5] — static link (enclosing scope's MP)
[MP+6..7] — function return value slot (0 if procedure)
[MP+8..]  — local variables
```

### Opcode Table

Each opcode is 1 byte, followed by 0–2 operand bytes as noted.

#### Stack / Constants
| Opcode | Mnemonic | Operands | Description |
|--------|----------|----------|-------------|
| $00 | LDCI b | 1: signed byte | Push 8-bit sign-extended integer constant |
| $01 | LDCW w | 2: word (lo,hi) | Push 16-bit integer constant |
| $02 | LDCC c | 1: byte | Push character constant |
| $03 | LDCB b | 1: 0/1 | Push boolean constant |
| $04 | LDCS len,... | 1+n bytes | Push string constant (len byte then chars) |
| $05 | LDCN | — | Push NIL pointer ($0000) |

#### Local Variable Access (relative to MP)
| Opcode | Mnemonic | Operands | Description |
|--------|----------|----------|-------------|
| $10 | LDL off | 1: byte offset | Push 16-bit local at MP+8+off |
| $11 | STL off | 1: byte offset | Pop into local at MP+8+off |
| $12 | LDA_L off | 1: byte offset | Push address of local (for var params) |
| $13 | LDB_L off | 1: byte offset | Push 8-bit (byte/char/bool) local |
| $14 | STB_L off | 1: byte offset | Pop byte into local |

#### Global Variable Access (relative to BASE)
| Opcode | Mnemonic | Operands | Description |
|--------|----------|----------|-------------|
| $18 | LDG off | 2: word offset | Push 16-bit global at BASE+off |
| $19 | STG off | 2: word offset | Pop into global |
| $1A | LDA_G off | 2: word offset | Push address of global |
| $1B | LDB_G off | 2: word offset | Push byte global |
| $1C | STB_G off | 2: word offset | Pop byte into global |

#### Indirect (via pointer on stack)
| Opcode | Mnemonic | Operands | Description |
|--------|----------|----------|-------------|
| $20 | LDIND | — | TOS=addr → pop addr, push 16-bit at addr |
| $21 | STIND | — | TOS=val, NOS=addr → pop both, store val at addr |
| $22 | LDB_IND | — | TOS=addr → pop, push byte at addr |
| $23 | STB_IND | — | TOS=val, NOS=addr → pop both, store byte at addr |
| $24 | INDEX w | 2: element size word | Array index: addr=NOS+TOS*w; push addr |

#### Integer Arithmetic
| Opcode | Mnemonic | Operands | Description |
|--------|----------|----------|-------------|
| $30 | ADI | — | pop b,a; push a+b |
| $31 | SBI | — | pop b,a; push a−b |
| $32 | MPI | — | pop b,a; push a*b |
| $33 | DVI | — | pop b,a; push a DIV b |
| $34 | MOD | — | pop b,a; push a MOD b |
| $35 | NGI | — | negate TOS |
| $36 | ABI | — | absolute value of TOS |
| $37 | SQI | — | TOS := TOS*TOS |

#### Bitwise / Boolean
| Opcode | Mnemonic | Operands | Description |
|--------|----------|----------|-------------|
| $38 | LAND | — | pop b,a; push a AND b (logical, 16-bit) |
| $39 | LOR | — | pop b,a; push a OR b |
| $3A | LNOT | — | push NOT TOS ($0000↔$FFFF) |
| $3B | BNOT | — | bitwise complement of TOS |

#### Comparison (push $FFFF=true, $0000=false)
| Opcode | Mnemonic | Operands | Description |
|--------|----------|----------|-------------|
| $40 | EQUI | — | a=b |
| $41 | NEQI | — | a<>b |
| $42 | LESI | — | a<b |
| $43 | LEQI | — | a<=b |
| $44 | GTRI | — | a>b |
| $45 | GEQI | — | a>=b |
| $46 | EQUB | — | byte/char/bool equality |
| $47 | EQUS | — | string equality (TOS=len-prefixed string addrs) |

#### Control Flow
| Opcode | Mnemonic | Operands | Description |
|--------|----------|----------|-------------|
| $50 | UJP off | 2: signed word | Unconditional jump IPC-relative |
| $51 | FJP off | 2: signed word | Jump if TOS=FALSE (0); pop |
| $52 | TJP off | 2: signed word | Jump if TOS=TRUE  (≠0); pop |
| $53 | IXPJP n,t0..tn | 1+2n: count+offsets | CASE jump table (indexed) |

#### Procedure/Function Calls
| Opcode | Mnemonic | Operands | Description |
|--------|----------|----------|-------------|
| $60 | CALL off | 2: word offset | Call procedure at IPC-relative offset (push activation record) |
| $61 | CALI addr | 2: absolute address | Call at absolute p-code address |
| $62 | RET | — | Return from procedure (restore IPC/MP) |
| $63 | RETF | — | Return from function (leave return value on stack) |
| $64 | MRKSTK n | 1: local-size byte | (Legacy) Reserve n bytes for locals, set up frame. Compiler no longer emits this — use MRKA instead. |
| $65 | DEPSTK | — | Discard locals, restore SP to frame base |
| $66 | STR | — | Pop word, store at MP+AR_RET_VAL (function result) |
| $67 | MRKA p,e | 2: pcount, lsize_extra | Mark+gather: pop pcount values from stack, set up new frame with those values as local slots 0..pcount-1, reserve lsize_extra extra bytes for body locals. Lets the caller evaluate args under its OWN MP. |

#### Heap
| Opcode | Mnemonic | Operands | Description |
|--------|----------|----------|-------------|
| $70 | NEW sz | 2: size word | Allocate sz bytes on heap; push pointer |
| $71 | DISP | — | TOS=ptr; free heap block (simple mark-release) |

#### I/O (maps to DOS/65 PEM + FARCALL)
| Opcode | Mnemonic | Operands | Description |
|--------|----------|----------|-------------|
| $80 | WRITI | — | Write integer (decimal) to console |
| $81 | WRITC | — | Write character |
| $82 | WRITB | — | Write boolean (TRUE/FALSE) |
| $83 | WRITS | — | Write string |
| $84 | WRITLN | — | Write newline (CR+LF) |
| $85 | READI | — | Read integer from console; push |
| $86 | READC | — | Read character; push |
| $87 | READS | — | Read string; push address |

#### Stack Manipulation
| Opcode | Mnemonic | Operands | Description |
|--------|----------|----------|-------------|
| $90 | DUP | — | Duplicate TOS |
| $91 | POP | — | Discard TOS |
| $92 | SWAP | — | Swap TOS and NOS |
| $93 | MOVS n | 1: count | Copy n bytes from NOS address to TOS address |

#### Halt
| Opcode | Mnemonic | Operands | Description |
|--------|----------|----------|-------------|
| $FF | HALT | — | Stop execution, return to DOS/65 |

---

## P-Code File Format (.PCD)

```
Offset  Size  Field
------  ----  -----
$00     2     Magic: $50 $43 ('PC')
$02     2     Version: $01 $00
$04     2     Code size (bytes)
$06     2     Global data size (bytes)
$08     2     String pool size (bytes)
$0A     2     Entry point offset (into code section)
$0C     n     Code section
$0C+n   m     Global data initial values
$0C+n+m p     String constants pool
```

The entire file must fit with the runtime in the $0800–$B7DF address range.

---

## Memory Map at Runtime (PRUN.COM)

```
$0000–$00FF  Zero page: p-machine registers + temporaries
$0100–$01FF  6502 hardware stack (subroutine calls within interpreter)
$0200–$07FF  DOS/65 system area (reserved)
$0800–$0BFF  PRUN interpreter code (~1 KB)
$0C00–$0FFF  Interpreter dispatch table + helper routines
$1000–$1FFF  P-code program code (loaded from .PCD)
$2000–$7FFF  P-machine value stack (grows upward from $2000)
$8000–$AFFF  Heap (grows downward from $B000)
$B000–$B7DF  String pool + global data area
```

---

## Compiler Memory Map (PASCAL.COM)

High-level sketch only; exact placement is controlled by `dos65.cfg` and
the source segment order. Current key anchors are `CPMDATA = $3400` and
`CODEBUF_BASE = $3E00`.

```
$0800–$0FFF  Compiler bootstrap / main loop
$1000–$2FFF  Lexer + scanner
$3000–$5FFF  Parser + AST builder (recursive descent)
$6000–$7FFF  Symbol table (hash table, lexical scopes)
$8000–$9FFF  Code generator (emit p-code)
$A000–$AFFF  String pool for identifiers/literals
$B000–$B7DF  I/O buffers (source file read, .PCD write)
```

---

## Source File Organization

```
software/pascal/
├── PLAN.md               ← this file
├── Makefile
├── dos65.cfg             ← linker config (copy from dbasic)
│
├── pascal.asm            ← compiler main entry + top-level driver
├── lexer.asm             ← tokenizer (scanner)
├── parser.asm            ← recursive-descent parser
├── symtab.asm            ← symbol table (scoped hash table)
├── codegen.asm           ← p-code emitter
├── zeropage.asm          ← zero-page variable declarations
├── definitions.asm       ← constants, macros, token codes
├── messages.asm          ← error strings
├── iolib.asm             ← DOS/65 console & file I/O wrappers
│
└── prun.asm              ← p-code runtime interpreter (PRUN.COM)
```

---

## Implementation Phases

### ✅ Phase 1 — P-Code Runtime (PRUN.COM)
- DOS/65 entry, `.PCD` load + magic validation
- Zero-page register layout (`prun.asm` + `zeropage.asm`)
- Main fetch-decode-execute loop with dispatch table
- Opcodes: LDCI/LDCW/LDCC/LDCB/LDCS/LDCN
- Opcodes: LDL/STL/LDA_L/LDB_L/STB_L (locals)
- Opcodes: LDG/STG/LDA_G/LDB_G/STB_G (globals)
- Opcodes: LDIND/STIND, LDB_IND/STB_IND, INDEX
- Opcodes: ADI/SBI/MPI/DVI/MOD/NGI (integer arithmetic)
- Opcodes: LAND/LOR/LNOT (boolean)
- Opcodes: EQUI/NEQI/LESI/LEQI/GTRI/GEQI/EQUB/EQUS (comparison)
- Opcodes: UJP/FJP/TJP (branches), CALL/RET/RETF/MRKSTK/DEPSTK/STR (procedures)
- Opcodes: WRITI/WRITC/WRITB/WRITS/WRITLN/READI/READC (I/O)
- Opcodes: DUP/POP/SWAP/MOVS (stack), HALT
- Opcodes: NEW/DISP (heap)

### ✅ Phase 2 — Compiler Lexer
- FCB-based sequential file reader with 128-byte sector buffer
- Token types, keyword table (linear scan)
- Number and string literal parsing
- Line/column tracking for error messages

### ✅ Phase 3 — Parser + Symbol Table (core)
- Recursive-descent parser, single-pass to p-code (no AST)
- Symbol table: linear array, scoped (global + procedure-local)
  - Symbol kinds: SYM_VAR, SYM_CONST, SYM_PROC, SYM_FUNC, SYM_TYPE, SYM_PARAM, SYM_RETVAL, SYM_VARREF
- `PROGRAM`, `VAR`, `CONST` declarations (global)
- `BEGIN … END` compound statements
- Assignment `:=`, expression parser with full precedence
- `WRITE`, `WRITELN`, `READ`, `READLN` built-ins
- `IF … THEN … ELSE`, `WHILE … DO`, `FOR … TO/DOWNTO … DO`
- `REPEAT … UNTIL`, `CASE … OF … END`
- `PROCEDURE` and `FUNCTION` declarations with local vars
- Value parameters and `VAR` (by-reference) parameters
- `TYPE` declarations (named type aliases)

### ✅ Phase 4 — Code Generator
- In-memory emit buffer (`CODEBUF_BASE = $3C00`)
- Emit routines for all opcodes
- Forward-jump backpatching (FJP/TJP/UJP patch slots)
- `.PCD` file header + sequential sector write
- Global variable allocation (`cg_globals`)

### ✅ Phase 5 — Integration (completed phases)
End-to-end compilation and execution verified for:
- `T01`–`T04`: basic integer, boolean, char I/O
- `T05`: string output
- `T06`–`T09`: arithmetic, comparisons
- `T10`–`T11`: IF/ELSE
- `T12`: WHILE loop
- `T13`: FOR loop
- `T14`: REPEAT/UNTIL
- `T15`: procedures with local vars
- `T16`–`T17`: functions and return values
- `T18`: VAR (by-reference) parameters
- `T19`/`T19B`: READ/READLN integer and char console input
- `T20`/`T20A`: TYPE declarations (named type aliases)
- `T21`/`T21A`/`T21B`/`T21C`: ARRAY types
- `T22`/`T22A`/`T22B`: RECORD types
- `T23`: STRING built-ins (`LENGTH`, `POS`, `COPY`, `CONCAT`)
- `T24`/`T24A`/`T25`: TEXT file I/O, `APPEND`, `EOF`, `EOLN`
- `T26`: pointers with `NEW`/`DISPOSE`
- `T27A`/`T27B`/`T27C`/`T27J`: inline, local, and nested RECORD coverage
- `T28`/`T28A`: `WITH` statements, including nested and comma-separated selectors
- `T29`/`T29A`: `SET OF` literals, membership, and set algebra
- `T30U`: standalone `UNIT ... INTERFACE ... IMPLEMENTATION ... END.` source form
- `T30V`/`T31`/`T31A`: `USES` cross-unit imports, including a unit's `IMPLEMENTATION` importing another unit
- `T32`/`T32A`/`T32B`/`T32C`: calls passing caller-local args (covers the `OP_MRKA` arg-gathering fix)
- `T33`/`T33A`/`T33B`/`T33C`/`T33D`/`T33E`: `REAL` arithmetic, coercions, functions, and `TEXT` file I/O

---

### ✅ Phase 10b — ARRAY Types
`ARRAY [lo..hi] OF basetype` for global vars; element size = 2 bytes.
Adjusted-offset trick: SYM_VAR offset stored as `raw_base - lo*2` so
`base + offset + i*2` lands on element `i`.
Tests: `T21`, `T21A`, `T21B`, `T21C`.

---

### ✅ Phase 11 — RECORD Types
Named record types via `TYPE T = RECORD …field-list… END;`, plus
inline `VAR P : RECORD …` and local record variables inside procs.
Scalar fields are 2 bytes; nested-record fields use the inner record's
own `record_size`.
Field-table at `field_table` (32 entries × 16 bytes); SYM_TYPE/SYM_VAR
entries store `first_field_idx` (byte 22) and `field_count` (byte 23).
Per-field nested-record metadata (inner `first_field`/`count`) lives in
parallel arrays `field_nested_first` / `field_nested_count`.
Field access compiles to `LDA_G/LDA_L base; { LDCI <off>; ADI }* ;
LDIND/STIND` — chained `r.outer.inner` walks one level per `.` and
only deref/store at the leaf scalar.
Tests: `T22`, `T22A`, `T22B`, plus `T27A` (inline records), `T27B`
(local record in a proc), `T27C` (named + anonymous nested records),
`T27D` (named nested field chain), and `T27J` (anonymous nested tail field).

Implementation notes:
- Field-name collection uses a dedicated `field_name_buf` so an inline
  `VAR x : RECORD …` no longer overwrites the outer variable name.
- The recursive `parse_type_spec` call inside the RECORD parser
  saves/restores outer `record_size`/`first_field`/`field_count` on
  the 6502 stack and snapshots the inner record's metadata into
  `nest_save_*` so each field gets its correct nested first/count.
- `parse_var_decls` allocates `record_size` bytes for local RECORD
  variables (still capped at one byte of local-AR offset for now).

---

### 🔲 Phase 12 — Quality of Life / Cleanup
- ✅ Removed debug `dbg_putc` scaffolding from `pascal.asm`; banner now reads `Compiling...` on its own line followed by `OK`
- ✅ `STRING` built-ins: `LENGTH`, `POS`, `COPY`, `CONCAT`
  - New opcodes `OP_LEN/POS/COPY/CONCAT` ($A0–$A3) handled in `prun.asm`
  - `COPY`/`CONCAT` results land in 3 round-robin work buffers at `$AD00/$AE00/$AF00`; deeply nested expressions can recycle a buffer before it's consumed
  - Test: `tests/t23.pas`
- ✅ `TEXT` file I/O: `ASSIGN`, `RESET`, `REWRITE`, `CLOSE`, `EOF`, `EOLN`; file-mode `WRITE`/`WRITELN`/`READ`/`READLN`
  - New type `TY_TEXT` ($08); each `TEXT` variable is a 168-byte struct (FCB 36 + buf 128 + mode/pos/eof/spare 4) allocated in the global area via `codegen_alloc_text_global`
  - New opcodes `OP_FASSGN/FRESET/FREWRT/FCLOSE/FWRC/FWRS/FWRI/FWLN/FRDC/FRDI/FRDLN/FEOF/FAPPND` ($B0–$BC), plus `FRDS/FWRB/FEOLN` ($BD–$BF) for STRING reads, BOOLEAN writes ("TRUE"/"FALSE"), and EOLN(F) testing. `APPEND` reuses the existing buffered TEXT path by scanning to EOF, keeping the final sector resident, and backing up the FCB next-record counter when the last record is only partially full.
  - Each file's struct embeds its own 128-byte sector buffer; runtime calls PEM `SETDMA` (fn 26) before each sector I/O so multiple files don't trample each other
  - `EOF(F)` uses 1-char lookahead — `RESET` and every `READ` peek the next byte, setting `F_EOF` on either CTRL-Z or PEM read-EOF, so `WHILE NOT EOF DO READ` consumes only real data
  - `EOLN(F)` peeks `buf[F_POS]` and returns true at CR/LF/EOF without consuming
  - `READ(F, S)` reads chars up to (not including) the next CR/LF into a fixed buffer at `$AC00`, storing the buffer pointer into the strvar (matching LDCS/CONCAT pointer semantics).  Stops at EOL without consuming so `READLN(F)` can advance past it
  - `WRITE`/`WRITELN` detect a `TEXT` first arg and switch to file mode (DUP file ptr, dispatch to `FWRC/FWRS/FWRB/FWRI`, terminate with `FWLN` or `POP`); `READ`/`READLN` peek the symtab to spot a `TEXT` first arg and route subsequent variables through `FRDC/FRDI/FRDS`
  - Filenames passed to `ASSIGN` are uppercased and split into 8.3 FCB form on the fly; closing a write-mode file pads the final partial sector with CTRL-Z
  - `TRUE`/`FALSE` recognized as predefined boolean constants in `parse_factor` (alongside built-in EOF/EOLN); emit `LDCB 1`/`LDCB 0` with `expr_type=TY_BOOL` so file/console writes route to `FWRB`/`WRITB`
  - Tests: `tests/t24.pas` (basic ops), `tests/t24a.pas` (`APPEND`), `tests/t25.pas` (STRING read, BOOLEAN write, EOLN)
- ✅ Heap allocation: `NEW`/`DISPOSE` for pointer-to-INTEGER (v1)
  - New type `TY_PTR` ($07); `^BASETYPE` parsed by `parse_type_spec` (base type code currently discarded — bump allocator always grants 2 bytes)
  - Opcodes `OP_NEW` ($70, inline 2-byte size) and `OP_DISP` ($71) wired into runtime; `OP_NEW` decrements `pm_np` by size and pushes the new heap address; `OP_DISP` is a no-op (bump allocator can't free)
  - `NEW(p)` parser pushes `&p` via `parse_arg_lvalue`, emits `OP_NEW 2` then `OP_STIND`; `DISPOSE(p)` parses an expression then emits `OP_DISP`
  - Pointer dereference: `p^` as rvalue routes through `@maybe_deref_ptr` after the `LDG`/`LDL` load (emits `OP_LDIND`, retypes to `TY_INT`); `p^ := expr` is a new branch in `@do_assign` that pushes the pointer value then `OP_STIND`s the RHS
  - Test: `tests/t26.pas`
- ✅ `WITH` statements for record selectors
  - `WITH record_expr DO stmt` and comma-separated selector lists (`WITH a, b DO ...`) are supported
  - Active `WITH` contexts are resolved innermost-first against record field tables; selected record base addresses are stored in hidden globals during code generation
  - Plain record-valued expressions now remain as addresses long enough for `WITH` and chained field selection to reuse their field metadata
  - Tests: `tests/t28.pas`, `tests/t28a.pas`
- ✅ `SET OF` types (v1: 16-bit masks over element values `0..15`)
  - New type `TY_SET` ($09), keywords `SET` and `IN`, and runtime opcode `OP_INSET` ($A4)
  - `SET OF lo..hi` is accepted when the declared bounds fit inside `0..15`; values are represented as 16-bit masks and stored like other 2-byte scalars
  - Set literals support `[]`, comma-separated items, and constant integer ranges such as `[1,3,5]` and `[3..6]`
  - `x IN s` emits `OP_INSET`; set union / difference / intersection compile through the existing bitwise ops as `+` / `-` / `*`
  - Tests: `tests/t29.pas`, `tests/t29a.pas`
- ✅ Standalone `UNIT` source form (v1)
  - Top-level parser now accepts `UNIT name; INTERFACE ... IMPLEMENTATION ... [BEGIN ...] END.`
  - INTERFACE supports exported `CONST`, `TYPE`, `VAR`, plus procedure/function headings
  - IMPLEMENTATION bodies rebind to interface-declared global routine entries instead of creating duplicates, so exported procedures/functions can be implemented later in the same source file
  - The optional final `BEGIN ... END` block serves as unit initialization code and becomes the entrypoint in the generated `.PCD`
  - Test: `tests/t30u.pas`
- ✅ Cross-unit `USES` / module imports (v1)
  - `PROGRAM`/`UNIT` sources may declare `USES unit1, unit2 ;` at the top of the
    main block (program) or within `INTERFACE` / `IMPLEMENTATION` (unit).
  - Imports compile inline into the current `.PCD`; per-unit init bodies run in
    declaration order, threaded through the startup-jump chain.
  - Tests: `tests/t30v.pas` (UNIT importing UNIT), `tests/t31.pas`,
    `tests/t31a.pas` (PROGRAM importing chained UNITs).
- ✅ Activation-record arg gathering via `OP_MRKA` ($67)
  - Bug fix: `OP_MRKSTK` changed `pm_mp` *before* the caller evaluated its
    args, so any `LDL`/`LDA_L` inside an arg expression read the *callee's*
    uninitialised frame instead of the caller's frame. All previous tests
    happened to pass only constants or globals as args, so the bug was latent
    until `t31a` exercised `TRIPLE(7)` whose body calls `TWICE(X)` with a
    local `X` — `TWICE` saw `X = 0` and the wrong total was returned.
  - Fix: new `OP_MRKA pcount, lsize_extra` opcode. The compiler now evaluates
    args first (under the caller's MP), pushes them to the value stack, then
    emits `MRKA`. At runtime `MRKA` shifts those `pcount` words up by
    `AR_LOCALS` so they land in local slots 0..pcount-1 of the new frame, and
    reserves `lsize_extra` more bytes for body locals. `OP_MRKSTK` is left in
    the runtime as a legacy entry but is no longer emitted.
  - `lsize_extra` is computed up-front and stashed on the 6502 hw stack across
    the arg loop so record-field lookups inside an arg (which scratch
    `fcall_lsize`) can't corrupt the MRKA operand.
  - Tests: `tests/t32.pas` (function-of-function with local arg),
    `tests/t32a.pas` (proc-of-proc with local arg + arithmetic),
    `tests/t32b.pas` (`VAR` by-ref param of a caller-local), `tests/t32c.pas`
    (callee with body locals beyond params, exercising non-zero `lsize_extra`).
- ✅ `REAL` type (v1: signed fixed-point, scale 100 / two decimal places)
  - New type `TY_REAL` ($0A), decimal literal token `TOK_REAL`, and runtime opcodes `OP_MPR`/`OP_DVR`/`OP_WRITR`/`OP_READR`/`FWRR`/`FRDR` for arithmetic and console / `TEXT` file I/O
  - Decimal literals compile to scaled 16-bit fixed-point integers; `/` yields `REAL`, while `DIV` / `MOD` remain integer-only
  - Mixed `INTEGER`/`REAL` assignments, parameter passing, comparisons, and arithmetic coerce automatically through the parser's numeric-coercion path
  - `WRITE` / `WRITELN` and `READ` / `READLN` support `REAL` for both console and `TEXT` files
  - Tests: `tests/t33.pas` (literals + arithmetic), `tests/t33a.pas` (functions + mixed compare/subtract), `tests/t33b.pas` (`TEXT` file write/read), `tests/t33c.pas` (literal console write), `tests/t33d.pas` (var assignment + write), `tests/t33e.pas` (mixed subtract in functions)
- Random-access typed files (`FILE OF X`) — not planned

---

## Remaining Feature Todo

* NONE


---

## DOS/65 Integration

### Invocation
```
PASCAL HELLO          ; compiles HELLO.PAS → HELLO.PCD
PRUN   HELLO          ; runs HELLO.PCD
```

The DOS/65 convention: the parsed argument FCB is at `DEFAULT_FCB = $0107` (not $005C
as in CP/M). The compiler forces extension to `.PAS`; the runtime forces `.PCD`.

### Console I/O
- Output: PEM fn 2 (CONOUT, character in A) — routes through DFT_CONSOLE driver
- Input:  PEM fn 1 (blocking read with echo)
- The `WRITELN` opcode emits CR ($0D) + LF ($0A)

### File I/O
- PEM fn 15 (`OPEN`), fn 16 (`CLOSE`), fn 20 (`READ`), fn 21 (`WRITE`), fn 22 (`MAKE`)
- Sequential 128-byte sector reads/writes via FCB in `$0900` area

---
## Key Design Constraints

| Constraint | Impact |
|------------|--------|
| 6502 has only A, X, Y registers | Interpreter inner loop uses X=opcode, A=scratch, Y for indexed ops |
| No hardware multiply/divide | MPI/DVI implemented as 16×16 software routines |
| Stack pointer limited (page 1 only) | P-machine stack lives in RAM $2000+, not page 1 |
| Max ~44 KB usable per .COM | Compiler is split from runtime; large Pascal programs can use full space |
| DOS/65 file names 8.3 uppercase | Compiler forces output filename to uppercase; error if too long |
| No OS multitasking | Interpreter is a tight loop; no context switching needed |

---

## Reference Material

- UCSD p-System IV.1 Opcodes: http://www.bitsavers.org/pdf/softech/
- Apple Pascal Internal Architecture: archive.org search "UCSD Pascal Internal Architecture"
- Pascal for Small Machines: http://pascal.hansotten.com/ucsd-p-system/
- p-Machine opcode PDF: http://pascal.hansotten.com/uploads/ucsd/wd/p_machine_opcode_1.pdf
- DOS/65 System Interface Guide: `/dos65_docs/DOS-65_System_Interface_Guide_A.pdf`
- DOS/65 ASM Manual: `/dos65_docs/DOS-65_ASM_Manual.pdf`
