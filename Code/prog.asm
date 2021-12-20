.text
.globl main
main:
  addi $sp, $sp, -8
  sw $ra, 4($sp)
  sw $fp, 8($sp)
  move $fp, $sp
  addi $sp, $sp, -8
# Start of Instr : Call of puts(Str Label str_0 , )
  la $v0, str_0
  addi $sp, $sp, -4
  sw $v0, 0($sp)
  jal puts
  addi $sp, $sp, 4
# Start of Instr : Call of puts(Str Label str_1 , )
  la $v0, str_1
  addi $sp, $sp, -4
  sw $v0, 0($sp)
  jal puts
  addi $sp, $sp, 4
# Start of Instr : Stack Space Reserved for Var (a) at -FP(0)
# Start of Instr : Variable : a = 20
  li $v0, 20
  sw $v0, 0($fp)
# Start of Instr : For 0 condition : (Variable a) 
# Increment :(Variable : a = Call of _sous_(Variable a , 1 , ))
for0:
  lw $v0 0($fp)
  beq $v0, $zero, end_for0
# Start of Instr : While 1 condition : (true) is true
while1:
  li $v0, 1
  beq $v0, $zero, end_while1
# Start of Instr : If 2 condition : (true) is true
  li $v0, 1
  beq $v0, $zero, else2
# Start of Instr : Break
  j end_while1
else2:
# Start of Instr : Stack Space Reserved for Var (r) at -FP(4)
# Start of Instr : Continue
  j while1
end_if2:
# Start of Instr : Call of puts(Str Label str_2 , )
  la $v0, str_2
  addi $sp, $sp, -4
  sw $v0, 0($sp)
  jal puts
  addi $sp, $sp, 4
  j while1
end_while1:
# Start of Instr : Continue
  j for_incre0
for_incre0:
  lw $v0 0($fp)
  addi $sp, $sp, -4
  sw $v0, 0($sp)
  li $v0, 1
  addi $sp, $sp, -4
  sw $v0, 0($sp)
  jal _sous_
  addi $sp, $sp, 8
  sw $v0, 0($fp)
  j for0
end_for0:
  move $sp, $fp
  lw $ra 4($sp)
  lw $fp 8($sp)
  jr $ra
puts:
  lw $a0 0($sp)
  li $v0, 4
  syscall
  jr $ra
_plus_:
  lw $t0 0($sp)
  lw $t1 4($sp)
  add $v0, $t1, $t0
  jr $ra
_sous_:
  lw $t0 0($sp)
  lw $t1 4($sp)
  sub $v0, $t1, $t0
  jr $ra
_mult_:
  lw $t0 0($sp)
  lw $t1 4($sp)
  mul $v0, $t0, $t1
  jr $ra
puti:
  lw $a0 0($sp)
  li $v0, 1
  syscall
  jr $ra
_div_:
  lw $t0 0($sp)
  lw $t1 4($sp)
  div $v0, $t1, $t0
  jr $ra
_mod_:
  lw $t0 0($sp)
  lw $t1 4($sp)
  rem $v0, $t1, $t0
  jr $ra

.data
str_1: .asciiz "
"
str_0: .asciiz "Should not print the Var A"
str_2: .asciiz "not_happening"
