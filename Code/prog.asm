.text
.globl main
main:
  addi $sp, $sp, -8
  sw $ra, 4($sp)
  sw $fp, 8($sp)
  move $fp, $sp
  addi $sp, $sp, -16
# Start of Instr : (Implicit) Stack Space Reserved for Var (a) at -FP(0)
# Start of Instr : Variable : a = 1000
  li $v0, 1000
  sw $v0, 0($fp)
# Start of Instr : (Implicit) Stack Space Reserved for Var (a) at -FP(4)
# Start of Instr : Variable : a = 1000
  li $v0, 1000
  sw $v0, -4($fp)
# Start of Instr : While 0 condition : (Variable a) is true
while0:
  lw $v0 -4($fp)
  beq $v0, $zero, end_while0
# Start of Instr : (Implicit) Stack Space Reserved for Var (r) at -FP(8)
# Start of Instr : Variable : r = Call of _.sous(Call of _.sous(Call of _.sous(Variable a , 5 , ) , 5 , ) , Call of _.mult(45 , 2 , ) , )
  lw $v0 -4($fp)
  addi $sp, $sp, -4
  sw $v0, 0($sp)
  li $v0, 5
  addi $sp, $sp, -4
  sw $v0, 0($sp)
  jal _.sous
  addi $sp, $sp, 8
  addi $sp, $sp, -4
  sw $v0, 0($sp)
  li $v0, 5
  addi $sp, $sp, -4
  sw $v0, 0($sp)
  jal _.sous
  addi $sp, $sp, 8
  addi $sp, $sp, -4
  sw $v0, 0($sp)
  li $v0, 45
  addi $sp, $sp, -4
  sw $v0, 0($sp)
  li $v0, 2
  addi $sp, $sp, -4
  sw $v0, 0($sp)
  jal _.mult
  addi $sp, $sp, 8
  addi $sp, $sp, -4
  sw $v0, 0($sp)
  jal _.sous
  addi $sp, $sp, 8
  sw $v0, -8($fp)
# Start of Instr : Call of _.puts(Str Label str_0 , )
  la $v0, str_0
  addi $sp, $sp, -4
  sw $v0, 0($sp)
  jal _.puts
  addi $sp, $sp, 4
# Start of Instr : Call of _.puti(Variable r , )
  lw $v0 -8($fp)
  addi $sp, $sp, -4
  sw $v0, 0($sp)
  jal _.puti
  addi $sp, $sp, 4
# Start of Instr : Call of _.puts(Str Label str_1 , )
  la $v0, str_1
  addi $sp, $sp, -4
  sw $v0, 0($sp)
  jal _.puts
  addi $sp, $sp, 4
# Start of Instr : While 1 condition : (Variable a) is true
while1:
  lw $v0 -4($fp)
  beq $v0, $zero, end_while1
# Start of Instr : Variable : a = Call of _.sous(Call of _.sous(Call of _.sous(Variable a , 5 , ) , 5 , ) , Call of _.mult(45 , 2 , ) , )
  lw $v0 -4($fp)
  addi $sp, $sp, -4
  sw $v0, 0($sp)
  li $v0, 5
  addi $sp, $sp, -4
  sw $v0, 0($sp)
  jal _.sous
  addi $sp, $sp, 8
  addi $sp, $sp, -4
  sw $v0, 0($sp)
  li $v0, 5
  addi $sp, $sp, -4
  sw $v0, 0($sp)
  jal _.sous
  addi $sp, $sp, 8
  addi $sp, $sp, -4
  sw $v0, 0($sp)
  li $v0, 45
  addi $sp, $sp, -4
  sw $v0, 0($sp)
  li $v0, 2
  addi $sp, $sp, -4
  sw $v0, 0($sp)
  jal _.mult
  addi $sp, $sp, 8
  addi $sp, $sp, -4
  sw $v0, 0($sp)
  jal _.sous
  addi $sp, $sp, 8
  sw $v0, -4($fp)
# Start of Instr : (Implicit) Stack Space Reserved for Var (t) at -FP(12)
# Start of Instr : Variable : t = Variable a
  lw $v0 -4($fp)
  sw $v0, -12($fp)
# Start of Instr : Call of _.puts(Str Label str_2 , )
  la $v0, str_2
  addi $sp, $sp, -4
  sw $v0, 0($sp)
  jal _.puts
  addi $sp, $sp, 4
# Start of Instr : Call of _.puti(Variable t , )
  lw $v0 -12($fp)
  addi $sp, $sp, -4
  sw $v0, 0($sp)
  jal _.puti
  addi $sp, $sp, 4
# Start of Instr : Call of _.puts(Str Label str_1 , )
  la $v0, str_1
  addi $sp, $sp, -4
  sw $v0, 0($sp)
  jal _.puts
  addi $sp, $sp, 4
  j while1
end_while1:
# Start of Instr : Call of _.puts(Str Label str_1 , )
  la $v0, str_1
  addi $sp, $sp, -4
  sw $v0, 0($sp)
  jal _.puts
  addi $sp, $sp, 4
# Start of Instr : Variable : a = Variable r
  lw $v0 -8($fp)
  sw $v0, -4($fp)
  j while0
end_while0:
  move $sp, $fp
  lw $ra 4($sp)
  lw $fp 8($sp)
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

.data
str_1: .asciiz "
"
str_0: .asciiz "r = "
str_2: .asciiz "t = "
