.text
.globl main
_add_aux:
  addi $sp, $sp, -8
  sw $ra, 4($sp)
  sw $fp, 8($sp)
  move $fp, $sp
  addi $sp, $sp, 0
# Start of Instr : Call of _.puts(Str Label str_0 , )
  la $v0, str_0
  addi $sp, $sp, -4
  sw $v0, 0($sp)
  jal _.puts
  addi $sp, $sp, 4
# Start of Instr : Call of _.puti(Variable a , )
  lw $v0 16($fp)
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
# Start of Instr : Call of _.puts(Str Label str_2 , )
  la $v0, str_2
  addi $sp, $sp, -4
  sw $v0, 0($sp)
  jal _.puts
  addi $sp, $sp, 4
# Start of Instr : Call of _.puti(Variable b , )
  lw $v0 12($fp)
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
# Start of Instr : Return Call of _.plus(Variable a , Variable b , )
  lw $v0 16($fp)
  addi $sp, $sp, -4
  sw $v0, 0($sp)
  lw $v0 12($fp)
  addi $sp, $sp, -4
  sw $v0, 0($sp)
  jal _.plus
  addi $sp, $sp, 8
  move $sp, $fp
  lw $ra 4($sp)
  lw $fp 8($sp)
  jr $ra
_add:
  addi $sp, $sp, -8
  sw $ra, 4($sp)
  sw $fp, 8($sp)
  move $fp, $sp
  addi $sp, $sp, 0
# Start of Instr : Call of _.puts(Str Label str_3 , )
  la $v0, str_3
  addi $sp, $sp, -4
  sw $v0, 0($sp)
  jal _.puts
  addi $sp, $sp, 4
# Start of Instr : Call of _.puti(Variable n , )
  lw $v0 12($fp)
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
# Start of Instr : Return Call of _.plus(Call of add_aux(4 , Variable n , ) , Call of add_aux(1 , Variable n , ) , )
  li $v0, 4
  addi $sp, $sp, -4
  sw $v0, 0($sp)
  lw $v0 12($fp)
  addi $sp, $sp, -4
  sw $v0, 0($sp)
  addi $sp, $sp, -4
  jal _add_aux
  addi $sp, $sp, -8
  addi $sp, $sp, -4
  sw $v0, 0($sp)
  li $v0, 1
  addi $sp, $sp, -4
  sw $v0, 0($sp)
  lw $v0 12($fp)
  addi $sp, $sp, -4
  sw $v0, 0($sp)
  addi $sp, $sp, -4
  jal _add_aux
  addi $sp, $sp, -8
  addi $sp, $sp, -4
  sw $v0, 0($sp)
  jal _.plus
  addi $sp, $sp, 8
  move $sp, $fp
  lw $ra 4($sp)
  lw $fp 8($sp)
  jr $ra
main:
_main:
  addi $sp, $sp, -8
  sw $ra, 4($sp)
  sw $fp, 8($sp)
  move $fp, $sp
  addi $sp, $sp, 0
# Start of Instr : Call of _.puti(Call of add(10 , ) , )
  li $v0, 10
  addi $sp, $sp, -4
  sw $v0, 0($sp)
  addi $sp, $sp, -4
  jal _add
  addi $sp, $sp, -4
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
  move $sp, $fp
  lw $ra 4($sp)
  lw $fp 8($sp)
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
str_0: .asciiz "A = "
str_2: .asciiz "B = "
str_3: .asciiz "add: n = "
