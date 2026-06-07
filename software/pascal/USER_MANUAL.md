# Pascal/65 User Manual

**A Pascal Compiler and P-Code Runtime for DOS/65 (PC6502)**

---

## Table of Contents

1. [Introduction](#1-introduction)
2. [Quick Start](#2-quick-start)
3. [Language Reference](#3-language-reference)
   - 3.1 [Program Structure](#31-program-structure)
   - 3.2 [Data Types](#32-data-types)
   - 3.3 [Declarations](#33-declarations)
   - 3.4 [Expressions](#34-expressions)
   - 3.5 [Statements](#35-statements)
   - 3.6 [Procedures and Functions](#36-procedures-and-functions)
   - 3.7 [Units and Modules](#37-units-and-modules)
   - 3.8 [Built-in Routines](#38-built-in-routines)
4. [File I/O](#4-file-io)
5. [Limitations](#5-limitations)
6. [Error Messages](#6-error-messages)
7. [Memory Layout](#7-memory-layout)
8. [P-Code File Format](#8-p-code-file-format)

---

## 1. Introduction

Pascal/65 is a Pascal compiler and interpreter toolchain for the **PC6502 running DOS/65**. It is
inspired by the UCSD p-System (1978), which successfully ran Pascal on Apple II and similar 6502
machines with 16–64 KB of RAM.

The toolchain consists of two separate programs:

| Program | Purpose |
|---------|---------|
| `PASCAL.COM` | Compiles a `.PAS` source file to a `.PCD` p-code bytecode file |
| `PRUN.COM` | Loads and executes a `.PCD` file on the p-code virtual machine |

Separating the compiler from the runtime allows each to use the full DOS/65 address space ($0800–$B7DF, ~44 KB). You can distribute `.PCD` files and run them without recompiling.

### Supported Pascal Subset

Pascal/65 implements a practical subset of standard Pascal including:

- All scalar types: INTEGER, CHAR, BOOLEAN, REAL (fixed-point)
- Structured types: ARRAY, RECORD, STRING, SET
- Pointer types with heap allocation (`NEW`/`DISPOSE`)
- Sequential TEXT file I/O
- Nested procedures and functions with lexical scoping
- Modular programming via `UNIT` and `USES`

---

## 2. Quick Start

### Compiling a Program

```
PASCAL HELLO
```

This reads `HELLO.PAS` from the current drive and writes `HELLO.PCD`.

### Running a Program

```
PRUN HELLO
```

This loads `HELLO.PCD` and executes it.

### Example: Hello, World

```pascal
PROGRAM HELLO;
BEGIN
  WRITELN('Hello, World!');
END.
```

Save as `HELLO.PAS`, then:

```
PASCAL HELLO
PRUN   HELLO
```

### Example: Simple Arithmetic

```pascal
PROGRAM CALC;
VAR
  A, B, SUM : INTEGER;
BEGIN
  A := 10;
  B := 25;
  SUM := A + B;
  WRITELN(SUM);
END.
```

---

## 3. Language Reference

### 3.1 Program Structure

A Pascal/65 program has this overall structure:

```pascal
PROGRAM name;
[ USES unit1, unit2; ]
[ CONST ... ]
[ TYPE  ... ]
[ VAR   ... ]
[ PROCEDURE/FUNCTION declarations ... ]
BEGIN
  { main statements }
END.
```

The `PROGRAM` header and the final `END.` (with a period) are required. Sections may appear in any order and may be repeated.

**Case sensitivity:** Keywords and identifiers are case-insensitive. `begin`, `BEGIN`, and `Begin` are all equivalent.

**Comments:** Use curly braces: `{ this is a comment }`. Comments may span multiple lines.

---

### 3.2 Data Types

#### INTEGER

16-bit signed integer. Range: −32768 to 32767.

```pascal
VAR N : INTEGER;
N := -1000;
```

#### CHAR

Single ASCII character (8-bit).

```pascal
VAR C : CHAR;
C := 'A';
```

Character literals are enclosed in single quotes. To represent a single quote character, double it:
`''''`.

#### BOOLEAN

Logical true or false. The predefined constants `TRUE` and `FALSE` are always available.

```pascal
VAR FLAG : BOOLEAN;
FLAG := TRUE;
IF FLAG THEN WRITELN('yes');
```

#### STRING

Variable-length string, maximum 255 characters. String literals are enclosed in single quotes.

```pascal
VAR S : STRING;
S := 'Hello';
S := S + ', World!';   { concatenation }
```

#### REAL

Signed fixed-point number with two decimal places. Stored as a scaled 16-bit integer
(value × 100). Range: approximately −327.67 to 327.67.

```pascal
VAR X : REAL;
X := 3.14;
X := X * 2.0;
WRITELN(X);           { prints 6.28 }
```

`INTEGER` values are automatically coerced to `REAL` in mixed expressions. The `/` operator always produces a `REAL` result; use `DIV` for integer division.

#### ARRAY

Fixed-size array with user-defined bounds.

```pascal
TYPE
  IntArray = ARRAY [1..10] OF INTEGER;
VAR
  A : IntArray;
  B : ARRAY [0..4] OF CHAR;
BEGIN
  A[1] := 42;
  B[0] := 'X';
END;
```

Bounds must be constant integer expressions. Element size is 2 bytes for all types.
Arrays may contain any scalar or structured base type.

#### RECORD

Aggregate type with named fields.

```pascal
TYPE
  Point = RECORD
    X : INTEGER;
    Y : INTEGER;
  END;
VAR
  P : Point;
BEGIN
  P.X := 10;
  P.Y := 20;
END;
```

Records may be nested: a field may itself be a record type.

```pascal
TYPE
  Rect = RECORD
    TopLeft : Point;
    BottomRight : Point;
  END;
VAR
  R : Rect;
BEGIN
  R.TopLeft.X := 0;
END;
```

#### SET

A set of small integers. The current implementation supports element values in the range 0..15 only; the value is stored as a 16-bit bitmask.

```pascal
TYPE
  Digits = SET OF 0..9;
VAR
  S : Digits;
BEGIN
  S := [1, 3, 5, 7, 9];           { odd digits }
  IF 3 IN S THEN WRITELN('yes');
  S := S + [2, 4];                 { union }
  S := S - [1];                    { difference }
  S := S * [2, 3, 4, 5];          { intersection }
END;
```

Set literals use square brackets. Ranges are supported: `[1..5]` is equivalent to `[1,2,3,4,5]`.
The empty set is `[]`.

#### Pointer Types

Pointer to a heap-allocated value.

```pascal
TYPE
  PInt = ^INTEGER;
VAR
  P : PInt;
BEGIN
  NEW(P);        { allocate on heap }
  P^ := 42;      { assign through pointer }
  WRITELN(P^);   { read through pointer }
  DISPOSE(P);    { release heap memory }
END;
```

The predefined constant `NIL` is a null pointer. Pointer types may point to any base type, including arrays and records.

#### TEXT

File variable for sequential text I/O. See [Section 4](#4-file-io).

---

### 3.3 Declarations

#### CONST

Named compile-time constants. Values may be integer, real, or boolean.

```pascal
CONST
  MAX_SIZE = 100;
  PI       = 3.14;
  DEBUG    = FALSE;
```

#### TYPE

Named type aliases.

```pascal
TYPE
  Index   = INTEGER;
  Name    = STRING;
  Matrix  = ARRAY [1..4] OF ARRAY [1..4] OF INTEGER;
```

#### VAR

Variable declarations. Multiple variables of the same type may be declared together with commas.

```pascal
VAR
  I, J, K : INTEGER;
  Name     : STRING;
  P        : ^INTEGER;
```

Variables declared at the top level (outside any procedure or function) are **global**. Variables declared inside a procedure or function are **local** to that scope and are not accessible outside it.

---

### 3.4 Expressions

#### Operator Precedence

From highest to lowest:

| Level | Operators | Notes |
|-------|-----------|-------|
| 1 (highest) | unary `-`, `NOT` | |
| 2 | `*`, `/`, `DIV`, `MOD`, `AND` | `*` on sets = intersection |
| 3 | `+`, `-`, `OR` | `+` on strings = concat; `+`/`-` on sets = union/difference | 
| 4 (lowest) | `=`, `<>`, `<`, `>`, `<=`, `>=`, `IN` | |

Use parentheses to override precedence.

#### Arithmetic Operators

| Operator | Types | Result | Description |
|----------|-------|--------|-------------|
| `+` | INT, REAL | same | Addition |
| `-` | INT, REAL | same | Subtraction |
| `*` | INT, REAL | same | Multiplication |
| `/` | INT, REAL | REAL | Division (always real) |
| `DIV` | INT | INT | Integer division (truncates toward zero) |
| `MOD` | INT | INT | Remainder after integer division |
| `-` (unary) | INT, REAL | same | Negation |

#### Boolean Operators

| Operator | Description |
|----------|-------------|
| `AND` | Logical and |
| `OR` | Logical or |
| `NOT` | Logical not |

#### Comparison Operators

All comparisons return `BOOLEAN`. Applies to INTEGER, REAL, CHAR, BOOLEAN, and STRING.

| Operator | Meaning |
|----------|---------|
| `=` | Equal |
| `<>` | Not equal |
| `<` | Less than |
| `>` | Greater than |
| `<=` | Less than or equal |
| `>=` | Greater than or equal |

#### String Concatenation

```pascal
S := 'Hello' + ', ' + 'World!';
```

#### Set Membership

```pascal
IF X IN S THEN ...    { true if integer X is an element of set S }
```

#### Pointer Dereference

```pascal
P^ := 10;    { store 10 at location pointed to by P }
X := P^;     { load value at location pointed to by P }
```

---

### 3.5 Statements

#### Assignment

```pascal
variable := expression;
```

The left-hand side may be a variable, an array element, a record field, or a pointer dereference.

```pascal
A[I]     := 5;
Rec.Field := 'X';
P^        := 99;
```

#### Compound Statement

```pascal
BEGIN
  stmt1;
  stmt2;
  ...
END
```

The semicolon is a **separator** between statements, not a terminator. No semicolon is needed before `END`, `ELSE`, or `UNTIL`.

#### IF

```pascal
IF condition THEN statement;

IF condition THEN
  statement
ELSE
  statement;
```

The `ELSE` branch is optional. In a chain of `IF`/`ELSE IF`, each `ELSE` binds to the nearest preceding `IF`.

#### WHILE

```pascal
WHILE condition DO statement;
```

Evaluates `condition` before each iteration. If the condition is initially false, the body is never executed.

```pascal
WHILE I <= 10 DO BEGIN
  WRITELN(I);
  I := I + 1;
END;
```

#### REPEAT

```pascal
REPEAT
  statement;
  ...
UNTIL condition;
```

Evaluates `condition` after each iteration. The body always executes at least once.

#### FOR

```pascal
FOR i := start TO end DO statement;
FOR i := start DOWNTO end DO statement;
```

The loop variable `i` must be an INTEGER. The loop body executes for each value from `start` to `end` inclusive. With `TO`, the variable increments by 1 each iteration; with `DOWNTO`, it decrements. If `start > end` (for `TO`) or `start < end` (for `DOWNTO`) the body is never executed.

**Note:** Do not modify the loop variable inside the body.

#### CASE

```pascal
CASE expression OF
  value1 : statement;
  value2 : statement;
  value3,
  value4 : statement;   { multiple values for one branch }
END;
```

`expression` must be INTEGER or CHAR. Each `value` must be a constant. If no case matches, execution continues after `END` (there is no `ELSE` branch).

#### WITH

`WITH` provides shorthand access to record fields:

```pascal
WITH record_variable DO
  statement;
```

Inside the statement, field names of `record_variable` may be used directly without the
`record_variable.` prefix.

Multiple records may be listed:

```pascal
WITH R1, R2 DO BEGIN
  X := R1.X + R2.X;    { or just: X := R1_field + R2_field }
END;
```

`WITH` contexts are resolved innermost-first for ambiguous field names.

---

### 3.6 Procedures and Functions

#### Declaring a Procedure

```pascal
PROCEDURE name [ (parameter-list) ];
[ VAR ... ]
[ CONST ... ]
[ TYPE ... ]
[ nested procedure/function declarations ]
BEGIN
  ...
END;
```

#### Declaring a Function

```pascal
FUNCTION name [ (parameter-list) ] : return-type;
[ VAR ... ]
BEGIN
  ...
  name := result;   { assign return value }
END;
```

The return value is set by assigning to the function's own name inside the body. A function must assign its name at least once on every code path.

#### Parameters

**Value parameters** — the caller's value is copied into the parameter:

```pascal
PROCEDURE DOUBLE(N : INTEGER);
BEGIN
  WRITELN(N * 2);
END;
```

**VAR parameters** — the caller passes the address of the variable; changes inside the procedure affect the caller's variable:

```pascal
PROCEDURE SWAP(VAR A, B : INTEGER);
VAR T : INTEGER;
BEGIN
  T := A;  A := B;  B := T;
END;
```

Multiple parameters of the same type may be listed together with commas. Parameters of different
types must be in separate groups.

**Maximum 8 parameters** per procedure or function.

#### Nested Procedures

Procedures and functions may be nested. A nested routine has access to all variables in its enclosing scopes (lexical/static scoping).

```pascal
PROCEDURE OUTER;
VAR X : INTEGER;

  PROCEDURE INNER;
  BEGIN
    X := X + 1;   { accesses OUTER's X }
  END;

BEGIN
  X := 0;
  INNER;
  WRITELN(X);   { prints 1 }
END;
```

#### Forward Declarations

There are no forward declarations in Pascal/65. All identifiers must be declared before use. Mutually recursive procedures can be structured by declaring the inner one first.

---

### 3.7 Units and Modules

Pascal/65 supports modular programming through **units**. A unit packages a set of declarations that can be imported by other programs or units.

#### Defining a Unit

```pascal
UNIT UnitName;

INTERFACE
  { Public declarations: CONST, TYPE, VAR, procedure/function headings }
  PROCEDURE Foo(N : INTEGER);
  FUNCTION  Bar : INTEGER;

IMPLEMENTATION
  { Private declarations and procedure/function bodies }
  PROCEDURE Foo(N : INTEGER);
  BEGIN
    ...
  END;

  FUNCTION Bar : INTEGER;
  BEGIN
    ...
  END;

[ BEGIN
    { Optional unit initialization code }
]
END.
```

- The `INTERFACE` section lists what is exported (visible to importers).
- The `IMPLEMENTATION` section contains the actual bodies.
- The optional `BEGIN...END` block at the end is initialization code that runs before the main program.
- Save the unit as `UnitName.PAS` (matching the unit name).

#### Importing a Unit

```pascal
PROGRAM MyProg;
USES UnitName;
BEGIN
  Foo(42);
END.
```

The `USES` clause must appear immediately after the `PROGRAM` (or `UNIT`) header, before other declarations. Multiple units may be listed:

```pascal
USES MathLib, StringLib, IoLib;
```

Units are compiled inline into the importing `.PCD` file. Each unit may itself have a `USES` clause to import other units. Circular imports are not supported.

---

### 3.8 Built-in Routines

#### Console Output

```pascal
WRITE(expr1, expr2, ...);      { write values without newline }
WRITELN(expr1, expr2, ...);    { write values then CR+LF }
WRITELN;                       { write CR+LF only }
```

Supported value types: `INTEGER`, `REAL`, `CHAR`, `BOOLEAN`, `STRING`.

`REAL` values are printed with exactly two decimal places: `3.14`, `-0.50`.

`BOOLEAN` values print as `TRUE` or `FALSE`.

#### Console Input

```pascal
READ(var1, var2, ...);     { read values }
READLN(var1, var2, ...);   { read values, then skip to next line }
READLN;                    { skip to next line }
```

Supported variable types: `INTEGER`, `REAL`, `CHAR`, `STRING`.

`INTEGER` input skips leading whitespace and reads digits (with optional leading `-`).
`CHAR` input reads the next character including whitespace.
`STRING` input reads up to a newline.

#### String Built-ins

```pascal
LENGTH(s)           { integer: number of characters in s }
POS(sub, s)         { integer: 1-based position of sub in s, 0 if not found }
COPY(s, start, len) { string: substring of s, starting at start (1-based), length len }
CONCAT(s1, s2, ...) { string: concatenation of all arguments }
```

`COPY` and `CONCAT` may also be written using the `+` operator on strings.

#### Heap Allocation

```pascal
NEW(p)      { allocate heap memory for the type that p points to; set p to address }
DISPOSE(p)  { release heap memory pointed to by p }
```

The current heap allocator is a simple bump allocator. `DISPOSE` is accepted by the compiler but does not actually reclaim memory.

---

## 4. File I/O

Pascal/65 supports sequential text file I/O through the `TEXT` type.

### Declaring a File Variable

```pascal
VAR
  F : TEXT;
```

### Opening Files

```pascal
ASSIGN(F, 'FILENAME.EXT');   { associate F with a DOS/65 filename }
RESET(F);                     { open F for reading (file must exist) }
REWRITE(F);                   { create/truncate F for writing }
APPEND(F);                    { open F for writing at end-of-file }
```

Filenames follow DOS/65 8.3 format (8-character name, 3-character extension, uppercase).
The filename passed to `ASSIGN` is automatically uppercased.

### Reading from a File

```pascal
READ(F, var);         { read next value from F into var }
READLN(F, var);       { read value, then skip to next line }
READLN(F);            { skip to next line }
```

Supported types: `CHAR`, `INTEGER`, `REAL`, `STRING`.

`EOF(F)` returns `TRUE` when the end of file has been reached. Typical read loop:

```pascal
RESET(F);
WHILE NOT EOF(F) DO BEGIN
  READ(F, Line);
  WRITELN(Line);
END;
CLOSE(F);
```

`EOLN(F)` returns `TRUE` if the next character is a line ending (CR or LF) or the file is at EOF, without consuming the character.

### Writing to a File

```pascal
WRITE(F, expr);        { write value to F }
WRITELN(F, expr);      { write value then newline to F }
WRITELN(F);            { write newline only }
```

Supported types: `CHAR`, `INTEGER`, `REAL`, `BOOLEAN`, `STRING`.

### Closing a File

```pascal
CLOSE(F);
```

Always close a file after writing. Closing a write-mode file flushes the final sector and pads it with CTRL-Z (the DOS/65 EOF marker).

### Complete File I/O Example

```pascal
PROGRAM FileDemo;
VAR
  F    : TEXT;
  Line : STRING;
  I    : INTEGER;
BEGIN
  { Write a file }
  ASSIGN(F, 'TEST.TXT');
  REWRITE(F);
  FOR I := 1 TO 5 DO BEGIN
    WRITE(F, 'Line ');
    WRITELN(F, I);
  END;
  CLOSE(F);

  { Read it back }
  RESET(F);
  WHILE NOT EOF(F) DO BEGIN
    READLN(F, Line);
    WRITELN(Line);
  END;
  CLOSE(F);
END.
```

---

## 5. Limitations

### Language Restrictions

- **No typed files.** `FILE OF T` (random-access typed files) is not implemented. Only sequential `TEXT` files are supported.
- **No forward declarations.** All symbols must be declared before use.
- **Maximum 8 parameters** per procedure or function.
- **SET elements 0..15 only.** Sets are stored as 16-bit bitmasks; element values above 15 are not supported.
- **REAL range is narrow.** Fixed-point REAL (scale 100) covers approximately −327.67 to 327.67 with exactly 2 decimal places. There is no floating-point support.
- **No multidimensional arrays.** `ARRAY [1..N] OF ARRAY [1..M] OF T` works, but there is no two-subscript syntax. Use a type alias for the inner array type.
- **No exception handling.** Runtime errors (e.g., division by zero) print a message and halt.
- **No GOTO.** The `GOTO` statement is not implemented.

### Runtime Restrictions

- **No bounds checking.** Array index out of bounds silently accesses wrong memory.
- **No nil-pointer check.** Dereferencing `NIL` causes undefined behavior.
- **Heap is not freed.** `DISPOSE` is a no-op; a program that allocates heavily will eventually exhaust heap memory.
- **Stack overflow is not detected.** Deep recursion may silently corrupt data.

### Compiler Restrictions

- **Identifier length:** up to 63 characters. Record field names: up to 12 characters.
- **Maximum ~32 KB of p-code** per program (code buffer limit).
- **Maximum ~6 KB symbol table** — very large programs with many globals or deeply nested scopes may exceed this.
- **Error recovery is basic.** After a syntax error, the compiler may emit spurious follow-on errors.

---

## 6. Error Messages

### Compiler Errors

The compiler prints errors in the form:

```
Error at line N col C: message
```

Common error messages:

| Message | Cause |
|---------|-------|
| `Expected ')'` | Missing closing parenthesis |
| `Expected ':'` | Missing colon in VAR declaration or CASE |
| `Expected ':='` | Missing assignment operator |
| `Expected 'BEGIN'` | Missing BEGIN keyword |
| `Expected 'END'` | Missing END keyword |
| `Expected identifier` | An identifier was required but not found |
| `Undefined identifier` | Name used but not declared |
| `Type mismatch` | Incompatible types in assignment or expression |
| `Too many params` | Procedure has more than 8 parameters |
| `Symbol table full` | Too many symbols declared |

After compilation completes, the compiler prints either:

```
OK
```

or

```
N error(s)
```

### Runtime Errors

The runtime prints errors in the form:

```
Runtime error: message
```

Common runtime errors:

| Message | Cause |
|---------|-------|
| `Div by zero` | Integer division or MOD with zero divisor |
| `Stack overflow` | P-machine value stack exhausted |
| `Heap exhausted` | No more heap memory available |
| `Bad PCD` | `.PCD` file has incorrect magic number or version |
| `File not found` | `RESET` failed because the file does not exist |
| `File error` | DOS/65 PEM returned an error during file I/O |

After a runtime error the program halts and returns to the DOS/65 command prompt.

---

## 7. Memory Layout

### Compiler (PASCAL.COM)

| Address | Contents |
|---------|----------|
| `$0800–$0FFF` | Compiler bootstrap and main loop |
| `$1000–$2FFF` | Lexer and scanner |
| `$3000–$5FFF` | Recursive-descent parser |
| `$6000–$7FFF` | Symbol table |
| `$8000–$9FFF` | Code generator and p-code emit buffer |
| `$A000–$AFFF` | String pool (identifiers and literals) |
| `$B000–$B7DF` | I/O buffers (source file input, .PCD output) |

### Runtime (PRUN.COM)

| Address | Contents |
|---------|----------|
| `$0000–$004F` | DOS/65 zero page (reserved) |
| `$0050–$0062` | P-machine registers (IPC, SP, MP, BASE, NP) |
| `$0100–$01FF` | 6502 hardware stack |
| `$0200–$07FF` | DOS/65 system area (reserved) |
| `$0800–$2FFF` | PRUN interpreter code and dispatch table |
| `$3000–$4FFF` | P-code loaded from `.PCD` |
| `$5000–$7FFF` | P-machine value stack (grows upward from `$5000`) |
| `$8000–$AFFF` | Heap (grows downward from `$B000`) |
| `$B000–$B7DF` | Global variables and string constants |

The p-machine value stack and heap grow toward each other. A program that uses very deep recursion (large stack) or heavy dynamic allocation (large heap) may cause them to collide.

---

## 8. P-Code File Format

The `.PCD` file produced by the compiler has a 12-byte header:

| Offset | Size | Field |
|--------|------|-------|
| `$00` | 2 | Magic: `$50 $43` (ASCII `PC`) |
| `$02` | 2 | Version: `$01 $00` |
| `$04` | 2 | Code section size (bytes) |
| `$06` | 2 | Global data size (bytes) |
| `$08` | 2 | String pool size (bytes) |
| `$0A` | 2 | Entry point offset (into code section) |
| `$0C` | *n* | Code section (p-code instructions) |
| `$0C+n` | *m* | Global data initial values |
| `$0C+n+m` | *p* | String constants pool |

The file is padded to 128-byte sector boundaries (DOS/65 requirement). The runtime validates the magic bytes and version before executing.

---

*Pascal/65 is inspired by the UCSD p-System. See PLAN.md for implementation notes and the full opcode reference.*
