.text
.globl main
_fibonacci:
  addi $sp, $sp, -4
# Start of Instr : (Implicit) Stack Space Reserved for Var (n) at -FP(0)
# Start of Instr : If 0 condition : (Call of _.or(Call of _.eq(Variable n , 0 , ) , Call of _.eq(Variable n , 1 , ) , )) is true
  addi $sp, $sp, -4
  sw $ra, 0($sp)
  addi $sp, $sp, -4
  sw $ra, 0($sp)
  lw $v0 0($fp)
  addi $sp, $sp, -4
  sw $v0, 0($sp)
  li $v0, 0
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
  li $v0, 1
  addi $sp, $sp, -4
  sw $v0, 0($sp)
  jal _.eq
  addi $sp, $sp, 8
  lw $ra 0($sp)
  addi $sp, $sp, 4
  addi $sp, $sp, -4
  sw $v0, 0($sp)
  jal _.or
  addi $sp, $sp, 8
  lw $ra 0($sp)
  addi $sp, $sp, 4
  beq $v0, $zero, else0
# Start of Instr : Return Variable n
  lw $v0 0($fp)
  jr $ra
else0:
# Start of Instr : Return Call of _.plus(Call of fibonacci(Call of _.sous(Variable n , 1 , ) , ) , Call of fibonacci(Call of _.sous(Variable n , 2 , ) , ) , )
  addi $sp, $sp, -4
  sw $ra, 0($sp)
  addi $sp, $sp, -8
  sw $ra, 0($sp)
  sw $fp, 4($sp)
  addi $s0, $sp, -4
  addi $sp, $sp, -4
  sw $ra, 0($sp)
  lw $v0 0($fp)
  addi $sp, $sp, -4
  sw $v0, 0($sp)
  li $v0, 1
  addi $sp, $sp, -4
  sw $v0, 0($sp)
  jal _.sous
  addi $sp, $sp, 8
  lw $ra 0($sp)
  addi $sp, $sp, 4
  addi $sp, $sp, -4
  sw $v0, 0($sp)
  move $sp, $s0
  move $fp, $sp
  jal _fibonacci
  addi $sp, $fp, 4
  lw $ra 0($sp)
  lw $fp 4($sp)
  addi $sp, $sp, 8
  addi $sp, $sp, -4
  sw $v0, 0($sp)
  addi $sp, $sp, -8
  sw $ra, 0($sp)
  sw $fp, 4($sp)
  addi $s0, $sp, -4
  addi $sp, $sp, -4
  sw $ra, 0($sp)
  lw $v0 0($fp)
  addi $sp, $sp, -4
  sw $v0, 0($sp)
  li $v0, 2
  addi $sp, $sp, -4
  sw $v0, 0($sp)
  jal _.sous
  addi $sp, $sp, 8
  lw $ra 0($sp)
  addi $sp, $sp, 4
  addi $sp, $sp, -4
  sw $v0, 0($sp)
  move $sp, $s0
  move $fp, $sp
  jal _fibonacci
  addi $sp, $fp, 4
  lw $ra 0($sp)
  lw $fp 4($sp)
  addi $sp, $sp, 8
  addi $sp, $sp, -4
  sw $v0, 0($sp)
  jal _.plus
  addi $sp, $sp, 8
  lw $ra 0($sp)
  addi $sp, $sp, 4
  jr $ra
end_if0:
  jr $ra
_main:
  addi $sp, $sp, 0
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
# Start of Instr : Call of _.puti(Call of fibonacci(25 , ) , )
  addi $sp, $sp, -4
  sw $ra, 0($sp)
  addi $sp, $sp, -8
  sw $ra, 0($sp)
  sw $fp, 4($sp)
  addi $s0, $sp, -4
  li $v0, 25
  addi $sp, $sp, -4
  sw $v0, 0($sp)
  move $sp, $s0
  move $fp, $sp
  jal _fibonacci
  addi $sp, $fp, 4
  lw $ra 0($sp)
  lw $fp 4($sp)
  addi $sp, $sp, 8
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
str_0: .asciiz "Fibo 25 = "
