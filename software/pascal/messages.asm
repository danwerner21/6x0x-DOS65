; messages.asm — compiler error and status strings (null-terminated)

        .segment  "CODE"

msg_banner:
        .BYTE   "Pascal/65 Compiler v0.1 for DOS/65",13,10,0
msg_compiling:
        .BYTE   "Compiling...",13,10,0
msg_ok:
        .BYTE   "OK",13,10,0
msg_errors:
        .BYTE   " error(s)",13,10,0
msg_line:
        .BYTE   " Line ",0
msg_col:
        .BYTE   " Col ",0

; Compiler errors
err_unexpected:
        .BYTE   "Unexpected token",0
err_expected_id:
        .byte   "IDENTIFIER expected",0
err_expected_eq:
        .byte   "'=' expected",0
err_expected_sm:
        .byte   "';' expected",0
err_expected_be:
        .byte   "'BEGIN' expected",0
err_expected_en:
        .byte "'END' expected",0
err_expected_do:
        .byte "'DO' expected",0
err_expected_of:
        .byte "'OF' expected",0
err_expected_th:
        .byte "'THEN' expected",0
err_expected_to:
        .byte "'TO' or 'DOWNTO' expected",0
err_expected_un:
        .byte "'UNTIL' expected",0
err_expected_lp:
        .byte "'(' expected",0
err_expected_rp:
        .byte "')' expected",0
err_expected_rb:
        .byte "']' expected",0
err_expected_co:
        .byte "':' expected",0
err_expected_as:
        .byte "':=' expected",0
err_undef:
        .byte "Undefined identifier",0
err_redef:
        .BYTE   "Identifier redefined",0
err_type:
        .BYTE   "Type mismatch",0
err_not_var:
        .BYTE   "Variable expected",0
err_not_proc:
        .BYTE   "Procedure/function expected",0
err_too_many:
        .BYTE   "Too many locals",0
err_overflow:
        .BYTE   "Integer overflow",0
err_string_long:
        .byte   "STRING too long",0
err_nofile:
        .byte "Cannot open source file",0
err_outfile:
        .BYTE   "Cannot create output file",0
err_pcd_full:
        .BYTE   "P-code buffer full",0

; Runtime errors (for PRUN.COM)
err_rt_magic:
        .BYTE   "Not a .PCD file",13,10,0
err_rt_stack:
        .BYTE   "Stack overflow",13,10,0
err_rt_div0:
        .BYTE   "Division by zero",13,10,0
err_rt_bounds:
        .BYTE   "Array out of bounds",13,10,0
err_rt_nil:
        .BYTE   "Nil pointer dereference",13,10,0
err_rt_opcode:
        .BYTE   "Unknown opcode ",0
msg_rt_done:
        .BYTE   13,10,"[Program exited]",13,10,0
