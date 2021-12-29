.text
.globl main
_println:
  addi $sp, $sp, -4
# Start of Instr : (Implicit) Stack Space Reserved for Var (to_print) at -FP(0)
# Start of Instr : Call of _.puts(Variable to_print , )
  addi $sp, $sp, -4
  sw $ra, 0($sp)
  lw $v0 0($fp)
  addi $sp, $sp, -4
  sw $v0, 0($sp)
  jal _.puts
  addi $sp, $sp, 4
  lw $ra 0($sp)
  addi $sp, $sp, 4
# Start of Instr : Call of _.puts(Str Label str_0 , )
  addi $sp, $sp, -4
  sw $ra, 0($sp)
  la $v0, str_0
  addi $sp, $sp, -4
  sw $v0, 0($sp)
  jal _.puts
  addi $sp, $sp, 4
  lw $ra 0($sp)
  addi $sp, $sp, 4
# Start of Instr : Return Void
  li $v0, 0
  jr $ra
_inf_or_equal:
  addi $sp, $sp, -8
# Start of Instr : (Implicit) Stack Space Reserved for Var (a) at -FP(0)
# Start of Instr : (Implicit) Stack Space Reserved for Var (b) at -FP(4)
# Start of Instr : If 6 condition : (Call of _.or(Call of _.eq(Variable a , Variable b , ) , Call of _.inf(Variable a , Variable b , ) , )) is true
  addi $sp, $sp, -4
  sw $ra, 0($sp)
  addi $sp, $sp, -4
  sw $ra, 0($sp)
  lw $v0 0($fp)
  addi $sp, $sp, -4
  sw $v0, 0($sp)
  lw $v0 -4($fp)
  addi $sp, $sp, -4
  sw $v0, 0($sp)
  jal _.eq
  addi $sp, $sp, 8
  lw $ra 0($sp)
  addi $sp, $sp, 4
  addi $sp, $sp, -4
  sw $v0, 0($sp)
  addi $sp, $sp, -4
  sw $ra, 0($sp)
  lw $v0 0($fp)
  addi $sp, $sp, -4
  sw $v0, 0($sp)
  lw $v0 -4($fp)
  addi $sp, $sp, -4
  sw $v0, 0($sp)
  jal _.inf
  addi $sp, $sp, 8
  lw $ra 0($sp)
  addi $sp, $sp, 4
  addi $sp, $sp, -4
  sw $v0, 0($sp)
  jal _.or
  addi $sp, $sp, 8
  lw $ra 0($sp)
  addi $sp, $sp, 4
  beq $v0, $zero, else7
# Start of Instr : Return true
  li $v0, 1
  jr $ra
else7:
end_if7:
# Start of Instr : Return false
  li $v0, 0
  jr $ra
_not:
  addi $sp, $sp, -4
# Start of Instr : (Implicit) Stack Space Reserved for Var (a) at -FP(0)
# Start of Instr : If 9 condition : (Call of _.and(Variable a , true , )) is true
  addi $sp, $sp, -4
  sw $ra, 0($sp)
  lw $v0 0($fp)
  addi $sp, $sp, -4
  sw $v0, 0($sp)
  li $v0, 1
  addi $sp, $sp, -4
  sw $v0, 0($sp)
  jal _.and
  addi $sp, $sp, 8
  lw $ra 0($sp)
  addi $sp, $sp, 4
  beq $v0, $zero, else10
# Start of Instr : Return false
  li $v0, 0
  jr $ra
else10:
end_if10:
# Start of Instr : Return true
  li $v0, 1
  jr $ra
_not_equal:
  addi $sp, $sp, -8
# Start of Instr : (Implicit) Stack Space Reserved for Var (a) at -FP(0)
# Start of Instr : (Implicit) Stack Space Reserved for Var (b) at -FP(4)
# Start of Instr : Return Call of not(Call of _.eq(Variable a , Variable b , ) , )
  addi $sp, $sp, -8
  sw $ra, 0($sp)
  sw $fp, 4($sp)
  addi $s0, $sp, -4
  addi $sp, $sp, -4
  sw $ra, 0($sp)
  lw $v0 0($fp)
  addi $sp, $sp, -4
  sw $v0, 0($sp)
  lw $v0 -4($fp)
  addi $sp, $sp, -4
  sw $v0, 0($sp)
  jal _.eq
  addi $sp, $sp, 8
  lw $ra 0($sp)
  addi $sp, $sp, 4
  addi $sp, $sp, -4
  sw $v0, 0($sp)
  move $sp, $s0
  move $fp, $sp
  jal _not
  addi $sp, $fp, 4
  lw $ra 0($sp)
  lw $fp 4($sp)
  addi $sp, $sp, 8
  jr $ra
_sup:
  addi $sp, $sp, -12
# Start of Instr : (Implicit) Stack Space Reserved for Var (a) at -FP(0)
# Start of Instr : (Implicit) Stack Space Reserved for Var (b) at -FP(4)
# Start of Instr : (Implicit) Stack Space Reserved for Var (result) at -FP(8)
# Start of Instr : Variable : result = Call of inf_or_equal(Variable a , Variable b , )
  addi $sp, $sp, -8
  sw $ra, 0($sp)
  sw $fp, 4($sp)
  addi $s0, $sp, -4
  lw $v0 0($fp)
  addi $sp, $sp, -4
  sw $v0, 0($sp)
  lw $v0 -4($fp)
  addi $sp, $sp, -4
  sw $v0, 0($sp)
  move $sp, $s0
  move $fp, $sp
  jal _inf_or_equal
  addi $sp, $fp, 4
  lw $ra 0($sp)
  lw $fp 4($sp)
  addi $sp, $sp, 8
  sw $v0, -8($fp)
# Start of Instr : Return Call of not(Variable result , )
  addi $sp, $sp, -8
  sw $ra, 0($sp)
  sw $fp, 4($sp)
  addi $s0, $sp, -4
  lw $v0 -8($fp)
  addi $sp, $sp, -4
  sw $v0, 0($sp)
  move $sp, $s0
  move $fp, $sp
  jal _not
  addi $sp, $fp, 4
  lw $ra 0($sp)
  lw $fp 4($sp)
  addi $sp, $sp, 8
  jr $ra
_sup_or_equal:
  addi $sp, $sp, -8
# Start of Instr : (Implicit) Stack Space Reserved for Var (a) at -FP(0)
# Start of Instr : (Implicit) Stack Space Reserved for Var (b) at -FP(4)
# Start of Instr : If 21 condition : (Call of _.or(Call of _.eq(Variable a , Variable b , ) , Call of sup(Variable a , Variable b , ) , )) is true
  addi $sp, $sp, -4
  sw $ra, 0($sp)
  addi $sp, $sp, -4
  sw $ra, 0($sp)
  lw $v0 0($fp)
  addi $sp, $sp, -4
  sw $v0, 0($sp)
  lw $v0 -4($fp)
  addi $sp, $sp, -4
  sw $v0, 0($sp)
  jal _.eq
  addi $sp, $sp, 8
  lw $ra 0($sp)
  addi $sp, $sp, 4
  addi $sp, $sp, -4
  sw $v0, 0($sp)
  addi $sp, $sp, -8
  sw $ra, 0($sp)
  sw $fp, 4($sp)
  addi $s0, $sp, -4
  lw $v0 0($fp)
  addi $sp, $sp, -4
  sw $v0, 0($sp)
  lw $v0 -4($fp)
  addi $sp, $sp, -4
  sw $v0, 0($sp)
  move $sp, $s0
  move $fp, $sp
  jal _sup
  addi $sp, $fp, 4
  lw $ra 0($sp)
  lw $fp 4($sp)
  addi $sp, $sp, 8
  addi $sp, $sp, -4
  sw $v0, 0($sp)
  jal _.or
  addi $sp, $sp, 8
  lw $ra 0($sp)
  addi $sp, $sp, 4
  beq $v0, $zero, else22
# Start of Instr : Return true
  li $v0, 1
  jr $ra
else22:
end_if22:
# Start of Instr : Return false
  li $v0, 0
  jr $ra
_print_bool:
  addi $sp, $sp, -4
# Start of Instr : (Implicit) Stack Space Reserved for Var (a) at -FP(0)
# Start of Instr : If 24 condition : (Variable a) is true
  lw $v0 0($fp)
  beq $v0, $zero, else25
# Start of Instr : Call of println(Str Label str_1 , )
  addi $sp, $sp, -8
  sw $ra, 0($sp)
  sw $fp, 4($sp)
  addi $s0, $sp, -4
  la $v0, str_1
  addi $sp, $sp, -4
  sw $v0, 0($sp)
  move $sp, $s0
  move $fp, $sp
  jal _println
  addi $sp, $fp, 4
  lw $ra 0($sp)
  lw $fp 4($sp)
  addi $sp, $sp, 8
  j end_if25
else25:
# Start of Instr : Call of println(Str Label str_2 , )
  addi $sp, $sp, -8
  sw $ra, 0($sp)
  sw $fp, 4($sp)
  addi $s0, $sp, -4
  la $v0, str_2
  addi $sp, $sp, -4
  sw $v0, 0($sp)
  move $sp, $s0
  move $fp, $sp
  jal _println
  addi $sp, $fp, 4
  lw $ra 0($sp)
  lw $fp 4($sp)
  addi $sp, $sp, 8
end_if25:
# Start of Instr : Return Void
  li $v0, 0
  jr $ra
_swap:
  addi $sp, $sp, -12
# Start of Instr : (Implicit) Stack Space Reserved for Var (a) at -FP(0)
# Start of Instr : (Implicit) Stack Space Reserved for Var (b) at -FP(4)
# Start of Instr : (Implicit) Stack Space Reserved for Var (temp) at -FP(8)
# Start of Instr : Variable : temp = Deref of Pointer (a) 
  lw $t0 0($fp)
  lw $v0 0($t0)
  sw $v0, -8($fp)
# Start of Instr : Variable : Derf Pointer a = Deref of Pointer (b) 
  lw $t0 -4($fp)
  lw $v0 0($t0)
  lw $t0 0($fp)
  sw $v0, 0($t0)
# Start of Instr : Variable : Derf Pointer b = Variable temp
  lw $v0 -8($fp)
  lw $t0 -4($fp)
  sw $v0, 0($t0)
# Start of Instr : Return Void
  li $v0, 0
  jr $ra
_main:
  addi $sp, $sp, 0
# Start of Instr : Call of print_bool(true , )
  addi $sp, $sp, -8
  sw $ra, 0($sp)
  sw $fp, 4($sp)
  addi $s0, $sp, -4
  li $v0, 1
  addi $sp, $sp, -4
  sw $v0, 0($sp)
  move $sp, $s0
  move $fp, $sp
  jal _print_bool
  addi $sp, $fp, 4
  lw $ra 0($sp)
  lw $fp 4($sp)
  addi $sp, $sp, 8
# Start of Instr : Return Void
  li $v0, 0
  jr $ra
main:
  addi $sp, $sp, -8
  sw $ra, 0($sp)
  sw $fp, 4($sp)
  addi $s0, $sp, -4
  move $sp, $s0
  move $fp, $sp
  jal _main
  addi $sp, $fp, 4
  lw $ra 0($sp)
  lw $fp 4($sp)
  addi $sp, $sp, 8
  jr $ra
_.puts:
  lw $a0 0($sp)
  li $v0, 4
  syscall
  jr $ra
_.plus:
  lw $t0 0($sp)
  lw $t1 4($sp)
  add $v0, $t1, $t0
  jr $ra
_.sous:
  lw $t0 0($sp)
  lw $t1 4($sp)
  sub $v0, $t1, $t0
  jr $ra
_.mult:
  lw $t0 0($sp)
  lw $t1 4($sp)
  mul $v0, $t0, $t1
  jr $ra
_.puti:
  lw $a0 0($sp)
  li $v0, 1
  syscall
  jr $ra
_.div:
  lw $t0 0($sp)
  lw $t1 4($sp)
  div $v0, $t1, $t0
  jr $ra
_.mod:
  lw $t0 0($sp)
  lw $t1 4($sp)
  rem $v0, $t1, $t0
  jr $ra
_.eq:
  lw $t0 0($sp)
  lw $t1 4($sp)
  seq $v0, $t1, $t0
  jr $ra
_.or:
  lw $t0 0($sp)
  lw $t1 4($sp)
  or $v0, $t1, $t0
  jr $ra
_.and:
  lw $t0 0($sp)
  lw $t1 4($sp)
  and $v0, $t1, $t0
  jr $ra
_.inf:
  lw $t0 0($sp)
  lw $t1 4($sp)
  slt $v0, $t1, $t0
  jr $ra

.data
str_0: .asciiz "
"
str_2: .asciiz "false"
str_1: .asciiz "true"
