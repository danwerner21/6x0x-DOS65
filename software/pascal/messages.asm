; messages.asm — compiler error and status strings (null-terminated)

.segment "CODE"

msg_banner:     .byte "Pascal/65 Compiler v0.1 for DOS/65",13,10,0
msg_compiling:  .byte "Compiling: ",0
msg_ok:         .byte "OK",13,10,0
msg_errors:     .byte " error(s)",13,10,0
msg_line:       .byte " Line ",0
msg_col:        .byte " Col ",0

; Compiler errors
err_unexpected: .byte "Unexpected token",0
err_expected_id:.byte "Identifier expected",0
err_expected_eq:.byte "'=' expected",0
err_expected_sm:.byte "';' expected",0
err_expected_be:.byte "'BEGIN' expected",0
err_expected_en:.byte "'END' expected",0
err_expected_do:.byte "'DO' expected",0
err_expected_of:.byte "'OF' expected",0
err_expected_th:.byte "'THEN' expected",0
err_expected_to:.byte "'TO' or 'DOWNTO' expected",0
err_expected_un:.byte "'UNTIL' expected",0
err_expected_lp:.byte "'(' expected",0
err_expected_rp:.byte "')' expected",0
err_expected_rb:.byte "']' expected",0
err_expected_co:.byte "':' expected",0
err_expected_as:.byte "':=' expected",0
err_undef:      .byte "Undefined identifier",0
err_redef:      .byte "Identifier redefined",0
err_type:       .byte "Type mismatch",0
err_not_var:    .byte "Variable expected",0
err_not_proc:   .byte "Procedure/function expected",0
err_too_many:   .byte "Too many locals",0
err_overflow:   .byte "Integer overflow",0
err_string_long:.byte "String too long",0
err_nofile:     .byte "Cannot open source file",0
err_outfile:    .byte "Cannot create output file",0
err_pcd_full:   .byte "P-code buffer full",0

; Runtime errors (for PRUN.COM)
err_rt_magic:   .byte "Not a .PCD file",13,10,0
err_rt_stack:   .byte "Stack overflow",13,10,0
err_rt_div0:    .byte "Division by zero",13,10,0
err_rt_bounds:  .byte "Array out of bounds",13,10,0
err_rt_nil:     .byte "Nil pointer dereference",13,10,0
err_rt_opcode:  .byte "Unknown opcode",13,10,0
msg_rt_done:    .byte 13,10,"[Program exited]",13,10,0
