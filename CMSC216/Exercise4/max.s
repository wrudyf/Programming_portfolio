# max(x, y)

max:
   # PROLOGUE
	subu    	$sp, $sp, 8   
	sw 		$ra, 8($sp)
	sw 		$fp, 4($sp)
	addu 		$fp, $sp, 8

	
   # BODY
	bgt		$a0, $a1, end	#if first num is greater than second
	li		$t0, 15
	move		$v0, $a1
	j		skip
end:
	li 		$t1, 20
	move 		$v0, $a0
skip:	
	
   # EPILOGUE
	move	 	$sp, $fp
	lw		$ra, ($fp)
	lw		$fp, -4($sp)
	jr		$ra
