   .data
str1:
   .asciiz "abba"
str2:
   .asciiz "racecar"
str3:
   .asciiz "swap paws",
str4:
   .asciiz "not a palindrome"
str5:
   .asciiz "another non palindrome"
str6:
   .asciiz "almost but tsomla"

# array of char pointers = {&str1, &str2, ..., &str6}
ptr_arr:
   .word str1, str2, str3, str4, str5, str6, 0

yes_str:
   .asciiz " --> Y\n"
no_str:
   .asciiz " --> N\n"

   .text

# main(): ##################################################
#   char ** j = ptr_arr
#   while (*j != 0):
#     rval = is_palindrome(*j)
#     printf("%s --> %c\n", *j, rval ? yes_str: no_str)
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

   la   $s0, ptr_arr        # use s0 for j. init ptr_arr
main_while:
   lw   $s1, ($s0)         # use s1 for *j
   beqz $s1, main_end      # while (*j != 0):
   move $a0, $s1           #    print_str(*j)
   li   $v0, 4
   syscall
   move $a0, $s1           #    v0 = is_palindrome(*j)
   jal  is_palindrome
   beqz $v0, main_print_no #    if v0 != 0:
   la   $a0, yes_str       #       print_str(yes_str)
   b    main_print_resp
main_print_no:             #    else:
   la   $a0, no_str        #       print_str(no_str)
main_print_resp:
   li   $v0, 4
   syscall

   addu $s0, $s0, 4       #     j++
   b    main_while        # end while
main_end:

   # EPILOGUE
   move $sp, $fp           # restore $sp
   lw   $ra, ($fp)         # restore saved $ra
   lw   $fp, -4($sp)       # restore saved $fp
   j    $ra                # return to kernel
# end main ################################################
#name: rudy f
#university id: 114096296
#umd directory id: rfuente5

is_palindrome:
	# PROLOGUE
	subu		$sp, $sp, 8
	sw		$ra, 8($sp)
	sw		$fp, 4($sp)
	addu		$fp, $sp, 8

	#BODY
	
	#first thing to do is copy our string address into two registers
	#one register will hold our starting point we increment from
	#other register will hold our end point we decrement from
	

	la		$t0, ($a0)	#reg to hold start of address
	la		$t1, ($a0)	#reg to hold end of address
	
	lb		$t2, ($t0)	#reg to hold our first char of string
	lb		$t3, ($t1)	#reg to hold our last char of string

	
	#we will get string length first here

	li		$t4, 0		#reg to hold length of string
	#loop to get string length
strlen:	beq		$t3, 0, endloop	#compare char to null byte 0
	add		$t4, $t4, 1	#increase length by 1
	addi		$t1, $t1, 1	#increase address in t1 by 1
	lb		$t3, ($t1)	#load next char into t3
	j		strlen
endloop:

	#after end loop, we should have strlen. now we divide strlen by 2
	la		$t1, ($t0)	#reset t1 to start of string
	lb		$t3, ($t1)	#get last char
	add		$t1, $t1, $t4	#add size to t1 to get to end of str
	sub		$t1, $t1, 1	#minus 1 to get to char before null
	div		$t4, $t4, 2	#divide t4 by 2 to get halfway point


	
	#now that we have half loop size, we add one to start of address up to
	#half, then we decrement from end down to half. doing this allows us
	#to compare start and end chars to see if they are equal
	
	li		$t5, 0		#register to hold outer loop iterator

	#outer is loop to increment/decrement string pointers
	#so we can get the char at start and end of string up to middle point
	
outer:	beq		$t5, $t4, done
	
	bne		$t2, $t3, false	#if start and end char not equal,
					#then jump to false
	
	add		$t5, $t5, 1	#increase half counter by 1
	add		$t0, $t0, 1	#increase start of address by 1
	sub		$t1, $t1, 1	#decrease end of address by 1

	lb		$t2, ($t0)	#load first char + 1
	lb		$t3, ($t1)	#load last char - 1
	
	j		outer
	
false:
	li		$v0, 0		#return 0 false
	j		end
done:	
	li		$v0, 1		#return 1 true
end:	
	
	#EPILOGUE
	move 		$sp, $fp
	lw		$ra, ($fp)
	lw		$fp, -4($sp)
	jr		$ra
