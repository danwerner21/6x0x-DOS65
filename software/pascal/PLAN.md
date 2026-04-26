# Pascal Compiler & P-Code Runtime for DOS/65 (PC6502)

## Overview

This project implements a two-component Pascal toolchain for the PC6502 running DOS/65:

1. **PASCAL.COM** — Pascal compiler: reads `.PAS` source → emits `.PCD` p-code bytecode
2. **PRUN.COM** — P-code runtime engine: reads `.PCD` → interprets/executes on the 6502

Both components are written in 6502 assembly (ca65), built with ld65, and distributed as
standard DOS/65 `.COM` files loaded at `$0800`.

The design is inspired by the UCSD Pascal p-System (1978), which successfully ran Pascal
on 6502 systems (Apple II, etc.) with 16–64 KB RAM. We implement a **simplified subset**
suitable for the DOS/65 memory model and the educational/retro context.

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

**Alternative considered:** Native-code compiler (Pascal → 6502 machine code). Rejected
because it is far harder to implement correctly, produces larger programs, and offers no
portability advantage on a single-target system.

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
- Integer: `+`, `−`, `*`, `DIV`, `MOD`
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

### Phase 2 additions (future)
- `REAL` (floating point via software)
- `SET OF` types
- `WITH` statement
- `GOTO` / `LABEL`
- Unit/module system (UCSD-style)
- File I/O (`TEXT`, `FILE OF`)

---

## P-Code Instruction Set

The p-machine is a **stack-based virtual machine**. All operands live on a value stack;
instructions pop inputs and push results.

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
| $64 | MRKSTK n | 1: local-size byte | Reserve n bytes for locals, set up frame |
| $65 | DEPSTK | — | Discard locals, restore SP to frame base |

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
- In-memory emit buffer (`CODEBUF_BASE = $3000`)
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
- `T19`: READ/READLN integer input
- `T20`/`T20A`: TYPE declarations (named type aliases) ← just fixed

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
(local record in a proc), `T27C` (named + anonymous nested records).

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
  - New opcodes `OP_FASSGN/FRESET/FREWRT/FCLOSE/FWRC/FWRS/FWRI/FWLN/FRDC/FRDI/FRDLN/FEOF` ($B0–$BB), plus `FRDS/FWRB/FEOLN` ($BD–$BF) for STRING reads, BOOLEAN writes ("TRUE"/"FALSE"), and EOLN(F) testing.  $BC is reserved (APPEND not implemented — would require PEM #35/#36 random-record I/O)
  - Each file's struct embeds its own 128-byte sector buffer; runtime calls PEM `SETDMA` (fn 26) before each sector I/O so multiple files don't trample each other
  - `EOF(F)` uses 1-char lookahead — `RESET` and every `READ` peek the next byte, setting `F_EOF` on either CTRL-Z or PEM read-EOF, so `WHILE NOT EOF DO READ` consumes only real data
  - `EOLN(F)` peeks `buf[F_POS]` and returns true at CR/LF/EOF without consuming
  - `READ(F, S)` reads chars up to (not including) the next CR/LF into a fixed buffer at `$AC00`, storing the buffer pointer into the strvar (matching LDCS/CONCAT pointer semantics).  Stops at EOL without consuming so `READLN(F)` can advance past it
  - `WRITE`/`WRITELN` detect a `TEXT` first arg and switch to file mode (DUP file ptr, dispatch to `FWRC/FWRS/FWRB/FWRI`, terminate with `FWLN` or `POP`); `READ`/`READLN` peek the symtab to spot a `TEXT` first arg and route subsequent variables through `FRDC/FRDI/FRDS`
  - Filenames passed to `ASSIGN` are uppercased and split into 8.3 FCB form on the fly; closing a write-mode file pads the final partial sector with CTRL-Z
  - `TRUE`/`FALSE` recognized as predefined boolean constants in `parse_factor` (alongside built-in EOF/EOLN); emit `LDCB 1`/`LDCB 0` with `expr_type=TY_BOOL` so file/console writes route to `FWRB`/`WRITB`
  - Tests: `tests/t24.pas` (basic ops), `tests/t25.pas` (STRING read, BOOLEAN write, EOLN)
- ✅ Heap allocation: `NEW`/`DISPOSE` for pointer-to-INTEGER (v1)
  - New type `TY_PTR` ($07); `^BASETYPE` parsed by `parse_type_spec` (base type code currently discarded — bump allocator always grants 2 bytes)
  - Opcodes `OP_NEW` ($70, inline 2-byte size) and `OP_DISP` ($71) wired into runtime; `OP_NEW` decrements `pm_np` by size and pushes the new heap address; `OP_DISP` is a no-op (bump allocator can't free)
  - `NEW(p)` parser pushes `&p` via `parse_arg_lvalue`, emits `OP_NEW 2` then `OP_STIND`; `DISPOSE(p)` parses an expression then emits `OP_DISP`
  - Pointer dereference: `p^` as rvalue routes through `@maybe_deref_ptr` after the `LDG`/`LDL` load (emits `OP_LDIND`, retypes to `TY_INT`); `p^ := expr` is a new branch in `@do_assign` that pushes the pointer value then `OP_STIND`s the RHS
  - Test: `tests/t26.pas`
- `REAL` type (future — 16.16 fixed-point or software float)
- Random-access typed files (`FILE OF X`) — not planned

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
