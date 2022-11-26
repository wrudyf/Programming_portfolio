   .data

# array terminated by 0 (which is not part of the array)
xarr:
   .word 1
   .word 2
   .word 3
   .word 4
   .word 10
   .word 11
   .word 12
   .word 13
   .word 14
   .word 16
   .word 18
   .word 20
   .word 24
   .word 0

   .text

# main(): ##################################################
#   uint* j = xarr
#   while (*j != 0):
#     printf(" %d\n", fibonacci(*j))
#     j++
#
main:
   li   $sp, 0x7ffffffc    # initialize $sp

   # PROLOGUE
   subu $sp, $sp, 8        # expand stack by 8 bytes
   sw   $ra, 8($sp)        # push $ra (ret addr, 4 bytes)
   sw   $fp, 4($sp)        # push $fp (4 bytes)
   addu $fp, $sp, 8        # set $fp to saved $ra

   subu $sp, $sp, 8        # save s0, s1 on stack before using them
   sw   $s0, 8($sp)        # push $s0
   sw   $s1, 4($sp)        # push $s1

   la   $s0, xarr          # use s0 for j. init to xarr
main_while:
   lw   $s1, ($s0)         # use s1 for *j
   beqz $s1, main_end      # if *j == 0 go to main_end
   move $a0, $s1
   jal  fibonacci          # result = fibonacci(*j)
   move $a0, $v0           # print_int(result)
   li   $v0, 1
   syscall
   li   $a0, 10            # print_char('\n')
   li   $v0, 11
   syscall
   addu $s0, $s0, 4        # j++
   b    main_while
main_end:
   lw   $s0, -8($fp)       # restore s0
   lw   $s1, -12($fp)      # restore s1

   # EPILOGUE
   move $sp, $fp           # restore $sp
   lw   $ra, ($fp)         # restore saved $ra
   lw   $fp, -4($sp)       # restore saved $fp
   j    $ra                # return to kernel
## end main #################################################
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

#	add		$s1, $s1, 1	#add 1 to get back to original val of
					#n

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
