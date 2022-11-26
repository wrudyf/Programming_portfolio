#name: rudy f
#uid: 114096296
#umd directory id: rfuente5

reverse_prefix_sum:
	
	#PROLOGUE
	subu		$sp, $sp, 8
	sw		$ra, 8($sp)
	sw		$fp, 4($sp)
	addu		$fp, $sp, 8
	
	#BODY
	subu		$sp, $sp, 8	#grow stack for rsp(arr + 1) and *arr
	
	li		$s2, 0		#initialize int r with 0
	lw		$s3, ($a0)	#set s3 equal to first int in array
	sw		$s3, 4($sp)	#save first int on stack
	
	beq		$s3, -1, retz	#go to return zero if *arr == -1

	add		$a0, $a0, 4	#increase addr by 4 to get next element
					#in array...get array ptr + 1
	
	jal		reverse_prefix_sum
					#do reverse_prefix_sum(arr + 1)

	sw		$v0, 8($sp)	#save reverse_prefix_sum(arr + 1) on stack
	lw		$s4, -8($fp)	#load reverse_prefix_sum(arr + 1)
	lw		$s3, 4($sp)	#load (int)*arr
	
	addu		$s2, $s4, $s3	#r = rsp(arr + 1) + (int)*arr

	sub		$a0, $a0, 4	#get back to first element in addr



	sw		$s2 0($a0)	#*arr = r
	
	move		$v0, $s2	#return (r)
	j		return
	
retz:
	li		$v0, 0
	j		return

return:	
	#EPILOGUE
	move		$sp, $fp
	lw		$ra, ($fp)
	lw		$fp, -4($sp)
	jr		$ra
	
