.text
.globl main
main:
  addi $sp, $sp, -8
  sw $ra, 4($sp)
  sw $fp, 8($sp)
  move $fp, $sp
# Start of Instr : Stack Space Reserved for Var (a) at -FP(0)
  addi $sp, $sp, -4
# Start of Instr : Variable : a = 1000
  li $v0, 1000
  sw $v0, 0($fp)
# Start of Instr : Stack Space Reserved for Var (a) at -FP(0)
  addi $sp, $sp, -4
# Start of Instr : Variable : a = 1000
  li $v0, 1000
  sw $v0, 0($fp)
# Start of Instr : While 0 condition : (Variable a) is true
while0:
  lw $v0 0($fp)
  beq $v0, $zero, end_while0
# Start of Instr : Stack Space Reserved for Var (r) at -FP(4)
  addi $sp, $sp, -4
# Start of Instr : Variable : r = Call of _sous_( Call of _sous_( Call of _sous_( Variable a 5 ) 5 ) Call of _mult_( 45 2 ) )
  lw $v0 0($fp)
  addi $sp, $sp, -4
  sw $v0, 0($sp)
  li $v0, 5
  addi $sp, $sp, -4
  sw $v0, 0($sp)
  jal _sous_
  addi $sp, $sp, 8
  addi $sp, $sp, -4
  sw $v0, 0($sp)
  li $v0, 5
  addi $sp, $sp, -4
  sw $v0, 0($sp)
  jal _sous_
  addi $sp, $sp, 8
  addi $sp, $sp, -4
  sw $v0, 0($sp)
  li $v0, 45
  addi $sp, $sp, -4
  sw $v0, 0($sp)
  li $v0, 2
  addi $sp, $sp, -4
  sw $v0, 0($sp)
  jal _mult_
  addi $sp, $sp, 8
  addi $sp, $sp, -4
  sw $v0, 0($sp)
  jal _sous_
  addi $sp, $sp, 8
  sw $v0, -4($fp)
# Start of Instr : Call of puts( Str Label str_0 )
  la $v0, str_0
  addi $sp, $sp, -4
  sw $v0, 0($sp)
  jal puts
  addi $sp, $sp, 4
# Start of Instr : Call of puti( Variable r )
  lw $v0 -4($fp)
  addi $sp, $sp, -4
  sw $v0, 0($sp)
  jal puti
  addi $sp, $sp, 4
# Start of Instr : Call of puts( Str Label str_1 )
  la $v0, str_1
  addi $sp, $sp, -4
  sw $v0, 0($sp)
  jal puts
  addi $sp, $sp, 4
# Start of Instr : While 2 condition : (Variable a) is true
while2:
  lw $v0 0($fp)
  beq $v0, $zero, end_while2
# Start of Instr : Variable : a = Call of _sous_( Call of _sous_( Call of _sous_( Variable a 5 ) 5 ) Call of _mult_( 45 2 ) )
  lw $v0 0($fp)
  addi $sp, $sp, -4
  sw $v0, 0($sp)
  li $v0, 5
  addi $sp, $sp, -4
  sw $v0, 0($sp)
  jal _sous_
  addi $sp, $sp, 8
  addi $sp, $sp, -4
  sw $v0, 0($sp)
  li $v0, 5
  addi $sp, $sp, -4
  sw $v0, 0($sp)
  jal _sous_
  addi $sp, $sp, 8
  addi $sp, $sp, -4
  sw $v0, 0($sp)
  li $v0, 45
  addi $sp, $sp, -4
  sw $v0, 0($sp)
  li $v0, 2
  addi $sp, $sp, -4
  sw $v0, 0($sp)
  jal _mult_
  addi $sp, $sp, 8
  addi $sp, $sp, -4
  sw $v0, 0($sp)
  jal _sous_
  addi $sp, $sp, 8
  sw $v0, 0($fp)
# Start of Instr : Stack Space Reserved for Var (t) at -FP(8)
  addi $sp, $sp, -4
# Start of Instr : Variable : t = Variable a
  lw $v0 0($fp)
  sw $v0, -8($fp)
# Start of Instr : Call of puts( Str Label str_2 )
  la $v0, str_2
  addi $sp, $sp, -4
  sw $v0, 0($sp)
  jal puts
  addi $sp, $sp, 4
# Start of Instr : Call of puti( Variable t )
  lw $v0 -8($fp)
  addi $sp, $sp, -4
  sw $v0, 0($sp)
  jal puti
  addi $sp, $sp, 4
# Start of Instr : Call of puts( Str Label str_1 )
  la $v0, str_1
  addi $sp, $sp, -4
  sw $v0, 0($sp)
  jal puts
  addi $sp, $sp, 4
  j while2
end_while2:
# Start of Instr : Call of puts( Str Label str_1 )
  la $v0, str_1
  addi $sp, $sp, -4
  sw $v0, 0($sp)
  jal puts
  addi $sp, $sp, 4
# Start of Instr : Variable : a = Variable r
  lw $v0 -4($fp)
  sw $v0, 0($fp)
  j while0
end_while0:
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
str_0: .asciiz "r = "
str_2: .asciiz "t = "
