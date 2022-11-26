#name: rudy f
#uid: 114096296
#umd directory id: rfuente5

isqrt:	
	#PROLOGUE
	subu		$sp, $sp, 8
	sw		$ra, 8($sp)
	sw		$fp, 4($sp)
	addu		$fp, $sp, 8

	#BODY

	bge		$a0, 2, recur	#if n >= 2, do some recursion
	move		$v0, $a0	#return n if n < 2
	j		return

recur:	
	subu		$sp, $sp, 8	#grow stack by 8 bytes for 2 ints
	sw		$a0, 4($sp)	#save value of arg n on stack

	div		$a0, $a0, 4	#divide n by 4
	jal		isqrt		#call isqrt with new value of n

	sw		$v0, 8($sp)	#save isqrt(n/4) on stack
	lw		$s3, -8($fp)	#small = isqrt ( n/4 )
	mul		$s3, $s3, 2	#small = isqrt(n/4) * 2

	add		$s4, $s3, 1	#large = small + 1

	move		$s6, $s4	#copy large to s6	
	
	mul		$s4, $s4, $s4	#large = large * large

	lw		$s5, 4($sp)	#getting value of n in a registry
	bgt		$s4, $s5, rets	#if large * large > n go to ret small

	move		$v0, $s6	#return large	
	j		return
	
rets:
	move 		$v0, $s3	#return small
	
	
	
return:	
	#EPILOGUE
	move		$sp, $fp
	lw		$ra, ($fp)
	lw		$fp, -4($sp)
	jr		$ra
