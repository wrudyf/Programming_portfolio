	default rel
	section .text
	global entry
entry:
	mov rax, 20
	mov r9, 20
	cmp r9, 0
	jg  end
	mov rax, 0
	div rax, r9
end:
	ret
