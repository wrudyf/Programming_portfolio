#name: rudy f
#uid: 114096296
#umd directory id: rfuente5

fibonacci:
	
	# PROLOGUE
	subu		$sp, $sp, 8	#stack frame is 8 bytes long
	sw		$ra, 8($sp)	#save return address
	sw		$fp, 4($sp)	#save old frame pointer
	addu		$fp, $sp, 8	#set up frame pointer
	
	#BODY
	beq		$a0, 0, retz	#if n is zero, jump to retz to return 0
	
	bge		$a0, 1, recur	#if n is greater than 1, go to recur

	li		$v0, 1		#if we didn't go to recur, we add 1 to
					#our output v0
	
	j		return		#jump to return
	
recur:
	subu		$sp, $sp, 8	#grow stack for value of a0 and n - 1
	sw		$a0, 4($sp)	#save value of a0, save n in stack

	#fib n - 1
	
	lw		$s1, 4($sp)	#load n into s1
	sub		$s1, $s1, 1	#calculate n-1 and put into s1
	move		$a0, $s1	#put n-1 value into a0
	jal		fibonacci	#call fibonacci with n-1

	move		$s2, $v0

	sw		$v0, 8($sp)	#save n-1 on stack
	
	#fib n - 2
	
	lw		$s1, 4($sp)	#load n into s1 again
	sub		$s1, $s1, 2	#calculate n-2 and put into s1
	move		$a0, $s1	#put n-2 into a0
	jal		fibonacci	#call fibonacci with n-2

	lw		$s1, -8($fp)

	add		$v0, $v0, $s1	#do f(n-2) + f(n-1)
	j		return
	
retz:	
	li		$v0, 0
	
return:
	
	#EPILOGUE
	move		$sp, $fp	
	lw		$ra, ($fp)
	lw		$fp, -4($sp)
	jr		$ra
