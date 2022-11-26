# strlen(str)
	
#increment address pointer
strlen:
   # PROLOGUE
	subu		$sp, $sp, 8
	sw		$ra, 8($sp)
	sw		$fp, 4($sp)
	addu		$fp, $sp, 8
   # BODY

	li		$t4, 0
#a0 is not char index that can be compared to 10
#load byte from a0 into new address (lbu)
#char can be represented as unsigned char, in c
	la		$t0, ($a0)	#start
	la		$t1, ($a0)	#end

	lb		$t2, ($t0)	#start char
	lb		$t3, ($t1)	#end char
			
loop:	beq		$t3,0, end
	add		$t4, $t4, 1
#to increment pointer do add to add 1 byte
	add		$t1, $t1, 1
	lb		$t3, ($t1)
	j		loop
end:
	la		$t1, ($t0)
	add		$t1, $t1, $t4
	sub		$t1, $t1, 1
	sub		$t1, $t1, 1
	
	lb		$a0, ($t0)	#print first char... load byte
	li		$v0, 11		#load v with value to print
	syscall				#do syscall to print


	li		$a0, 10		#print newline
	li		$v0, 11
	syscall

	li		$a0, 43		#print +
	li		$v0, 11
	syscall
	
	lb		$a0, ($t1)	#print last char
	li		$v0, 11
	syscall
	
	li		$a0, 10		#print newline char
	li		$v0, 11	
	syscall

	li		$a0, 10		#print newline char
	li		$v0, 11
	syscall
	
	div		$t4, $t4, 2
	li		$t6, 0

sloop:	beq		$t6, $t4, end2
	lb		$a0, ($t0)
	li		$v0, 11
	syscall
	add		$t0, $t0, 1
	add		$t6, $t6, 1
	j		sloop

end2:
	li		$a0, 10		#print newline char
	li		$v0, 11
	syscall
	
	move		$v0, $t4
	
	
   # EPILOGUE
	move 		$sp, $fp
	lw		$ra, ($fp)
	lw		$fp, -4($sp)
	jr		$ra
