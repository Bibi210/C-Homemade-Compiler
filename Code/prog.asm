.text
.globl main
__test:
  addi $sp, $sp, -4
# Start of Instr : (Implicit) Stack Space Reserved for Var (a) at -FP(0)
# Start of Instr : Variable : Derf Pointer a = -37
  li $v0, -37
  lw $t0 0($fp)
  sw $v0, 0($t0)
# Start of Instr : Return Void
  li $v0, 0
  jr $ra
_main:
  addi $sp, $sp, -8
# Start of Instr : (Implicit) Stack Space Reserved for Var (c) at -FP(0)
# Start of Instr : Variable : c = 42
  li $v0, 42
  sw $v0, 0($fp)
# Start of Instr : (Implicit) Stack Space Reserved for Var (d) at -FP(4)
# Start of Instr : Variable : d = Taking ref of (c) 
  la $v0, 0($fp)
  sw $v0, -4($fp)
# Start of Instr : Call of _test(Taking ref of (c)  , )
  addi $sp, $sp, -8
  sw $ra, 0($sp)
  sw $fp, 4($sp)
  addi $s0, $sp, -4
  la $v0, 0($fp)
  addi $sp, $sp, -4
  sw $v0, 0($sp)
  move $sp, $s0
  move $fp, $sp
  jal __test
  addi $sp, $fp, 4
  lw $ra 0($sp)
  lw $fp 4($sp)
  addi $sp, $sp, 8
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
# Start of Instr : Call of _.puti(Deref of Pointer (d)  , )
  addi $sp, $sp, -4
  sw $ra, 0($sp)
  lw $t0 -4($fp)
  lw $v0 0($t0)
  addi $sp, $sp, -4
  sw $v0, 0($sp)
  jal _.puti
  addi $sp, $sp, 4
  lw $ra 0($sp)
  addi $sp, $sp, 4
# Start of Instr : Call of _.puts(Str Label str_1 , )
  addi $sp, $sp, -4
  sw $ra, 0($sp)
  la $v0, str_1
  addi $sp, $sp, -4
  sw $v0, 0($sp)
  jal _.puts
  addi $sp, $sp, 4
  lw $ra 0($sp)
  addi $sp, $sp, 4
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

.data
str_1: .asciiz "
"
str_0: .asciiz "c = "
