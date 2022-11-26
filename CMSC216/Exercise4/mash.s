# mash(x, y)

mash:
   # PROLOGUE
	subu		$sp, $sp, 8
	sw		$ra, 8($sp)
	sw		$fp, 4($sp)
	addu		$fp, $sp, 8
   # BODY
	sub		$sp, $sp, 4	#grow stack for an int
	li		$t2, 10		#set int to 10
	sw		$t0, 4($sp)	#sw store word, AKA store int

	lw 		$t3, 4($fp)	#t3 = x
	lw		$t4, 8($fp)	#t4 = y
	mul		$t5, $t2, $t3	#t5 = 10 * x
	add		$t6, $t5, $t4	#t5 = t5 + y
	move		$v0, $t6
	
	
   # EPILOGUE
	move		$sp, $fp
	lw		$ra, ($fp)
	lw		$fp, -4($sp)
	jr		$ra
