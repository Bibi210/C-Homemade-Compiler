.text
.globl main
main:
  li $v0, 0
  jr $ra
puts:
  lw $a0 0($sp)
  li $v0, 4
  syscall
  jr $ra
_plus_:
  lw $t0 0($sp)
  lw $t1 4($sp)
  add $v0, $t0, $t1
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

.data
