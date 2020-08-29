	; .file	"peace.c"
	.text
	.section	.rodata
.String0:
	.string	"peace"
	.text
	.globl	main
	.type	main, @function
main:
	endbr64
	pushq	%rbp
	movq	%rsp, %rbp
	leaq	.String0(%rip), %rdi
	call	puts@PLT
	movl	$0, %eax
	popq	%rbp
	ret
; 	.size	main, .-main
; 	.ident	"GCC: (Ubuntu 9.3.0-10ubuntu2) 9.3.0"
; 	.section	.note.GNU-stack,"",@progbits
; 	.section	.note.gnu.property,"a"
; 	.align 8
; 	.long	 1f - 0f
; 	.long	 4f - 1f
; 	.long	 5
; 0:
; 	.string	 "GNU"
; 1:
; 	.align 8
; 	.long	 0xc0000002
; 	.long	 3f - 2f
; 2:
; 	.long	 0x3
; 3:
; 	.align 8
; 4:
