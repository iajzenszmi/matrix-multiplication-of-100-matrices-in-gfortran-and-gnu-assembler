	.file	"matmule.f"
	.text
	.section	.rodata
.LC1:
	.string	"matmule.f"
.LC2:
	.ascii	"Matrix A:"
.LC3:
	.ascii	"Matrix B:"
.LC4:
	.ascii	"Matrix C:"
.LC5:
	.ascii	"Matrix D:"
	.text
	.type	MAIN__, @function
MAIN__:
.LFB0:
	.cfi_startproc
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register 6
	subq	$1024, %rsp
	movl	$1, -4(%rbp)
.L5:
	cmpl	$4, -4(%rbp)
	setg	%al
	movzbl	%al, %eax
	testl	%eax, %eax
	jne	.L46
	movl	$1, -8(%rbp)
.L4:
	cmpl	$3, -8(%rbp)
	setg	%al
	movzbl	%al, %eax
	testl	%eax, %eax
	jne	.L47
	movl	-4(%rbp), %edx
	movl	-8(%rbp), %eax
	addl	%eax, %edx
	movl	-8(%rbp), %eax
	cltq
	leaq	0(,%rax,4), %rcx
	movl	-4(%rbp), %eax
	cltq
	addq	%rcx, %rax
	subq	$5, %rax
	pxor	%xmm0, %xmm0
	cvtsi2ssl	%edx, %xmm0
	movss	%xmm0, -64(%rbp,%rax,4)
	addl	$1, -8(%rbp)
	jmp	.L4
.L47:
	nop
	addl	$1, -4(%rbp)
	jmp	.L5
.L46:
	nop
	movl	$1, -4(%rbp)
.L9:
	cmpl	$3, -4(%rbp)
	setg	%al
	movzbl	%al, %eax
	testl	%eax, %eax
	jne	.L48
	movl	$1, -8(%rbp)
.L8:
	cmpl	$5, -8(%rbp)
	setg	%al
	movzbl	%al, %eax
	testl	%eax, %eax
	jne	.L49
	movl	-4(%rbp), %eax
	imull	-8(%rbp), %eax
	movl	%eax, %ecx
	movl	-8(%rbp), %eax
	movslq	%eax, %rdx
	movq	%rdx, %rax
	addq	%rax, %rax
	addq	%rax, %rdx
	movl	-4(%rbp), %eax
	cltq
	addq	%rdx, %rax
	subq	$4, %rax
	pxor	%xmm0, %xmm0
	cvtsi2ssl	%ecx, %xmm0
	movss	%xmm0, -128(%rbp,%rax,4)
	addl	$1, -8(%rbp)
	jmp	.L8
.L49:
	nop
	addl	$1, -4(%rbp)
	jmp	.L9
.L48:
	nop
	movl	$1, -4(%rbp)
.L13:
	cmpl	$5, -4(%rbp)
	setg	%al
	movzbl	%al, %eax
	testl	%eax, %eax
	jne	.L50
	movl	$1, -8(%rbp)
.L12:
	cmpl	$8, -8(%rbp)
	setg	%al
	movzbl	%al, %eax
	testl	%eax, %eax
	jne	.L51
	movl	-4(%rbp), %eax
	subl	-8(%rbp), %eax
	movl	%eax, %ecx
	movl	-8(%rbp), %eax
	movslq	%eax, %rdx
	movq	%rdx, %rax
	salq	$2, %rax
	addq	%rax, %rdx
	movl	-4(%rbp), %eax
	cltq
	addq	%rdx, %rax
	subq	$6, %rax
	pxor	%xmm0, %xmm0
	cvtsi2ssl	%ecx, %xmm0
	movss	%xmm0, -288(%rbp,%rax,4)
	addl	$1, -8(%rbp)
	jmp	.L12
.L51:
	nop
	addl	$1, -4(%rbp)
	jmp	.L13
.L50:
	nop
	movl	$1, -4(%rbp)
.L19:
	cmpl	$4, -4(%rbp)
	setg	%al
	movzbl	%al, %eax
	testl	%eax, %eax
	jne	.L52
	movl	$1, -8(%rbp)
.L18:
	cmpl	$5, -8(%rbp)
	setg	%al
	movzbl	%al, %eax
	testl	%eax, %eax
	jne	.L53
	movl	-8(%rbp), %eax
	cltq
	leaq	0(,%rax,4), %rdx
	movl	-4(%rbp), %eax
	cltq
	addq	%rdx, %rax
	subq	$5, %rax
	pxor	%xmm0, %xmm0
	movss	%xmm0, -496(%rbp,%rax,4)
	movl	$1, -12(%rbp)
.L17:
	cmpl	$3, -12(%rbp)
	setg	%al
	movzbl	%al, %eax
	testl	%eax, %eax
	jne	.L54
	movl	-8(%rbp), %eax
	cltq
	leaq	0(,%rax,4), %rdx
	movl	-4(%rbp), %eax
	cltq
	addq	%rdx, %rax
	subq	$5, %rax
	movss	-496(%rbp,%rax,4), %xmm1
	movl	-12(%rbp), %eax
	cltq
	leaq	0(,%rax,4), %rdx
	movl	-4(%rbp), %eax
	cltq
	addq	%rdx, %rax
	subq	$5, %rax
	movss	-64(%rbp,%rax,4), %xmm2
	movl	-8(%rbp), %eax
	movslq	%eax, %rdx
	movq	%rdx, %rax
	addq	%rax, %rax
	addq	%rax, %rdx
	movl	-12(%rbp), %eax
	cltq
	addq	%rdx, %rax
	subq	$4, %rax
	movss	-128(%rbp,%rax,4), %xmm0
	mulss	%xmm2, %xmm0
	movl	-8(%rbp), %eax
	cltq
	leaq	0(,%rax,4), %rdx
	movl	-4(%rbp), %eax
	cltq
	addq	%rdx, %rax
	subq	$5, %rax
	addss	%xmm1, %xmm0
	movss	%xmm0, -496(%rbp,%rax,4)
	addl	$1, -12(%rbp)
	jmp	.L17
.L54:
	nop
	addl	$1, -8(%rbp)
	jmp	.L18
.L53:
	nop
	addl	$1, -4(%rbp)
	jmp	.L19
.L52:
	nop
	movl	$1, -4(%rbp)
.L25:
	cmpl	$4, -4(%rbp)
	setg	%al
	movzbl	%al, %eax
	testl	%eax, %eax
	jne	.L55
	movl	$1, -8(%rbp)
.L24:
	cmpl	$8, -8(%rbp)
	setg	%al
	movzbl	%al, %eax
	testl	%eax, %eax
	jne	.L56
	movl	-8(%rbp), %eax
	cltq
	leaq	0(,%rax,4), %rdx
	movl	-4(%rbp), %eax
	cltq
	addq	%rdx, %rax
	subq	$5, %rax
	pxor	%xmm0, %xmm0
	movss	%xmm0, -416(%rbp,%rax,4)
	movl	$1, -12(%rbp)
.L23:
	cmpl	$5, -12(%rbp)
	setg	%al
	movzbl	%al, %eax
	testl	%eax, %eax
	jne	.L57
	movl	-8(%rbp), %eax
	cltq
	leaq	0(,%rax,4), %rdx
	movl	-4(%rbp), %eax
	cltq
	addq	%rdx, %rax
	subq	$5, %rax
	movss	-416(%rbp,%rax,4), %xmm1
	movl	-12(%rbp), %eax
	cltq
	leaq	0(,%rax,4), %rdx
	movl	-4(%rbp), %eax
	cltq
	addq	%rdx, %rax
	subq	$5, %rax
	movss	-496(%rbp,%rax,4), %xmm2
	movl	-8(%rbp), %eax
	movslq	%eax, %rdx
	movq	%rdx, %rax
	salq	$2, %rax
	addq	%rax, %rdx
	movl	-12(%rbp), %eax
	cltq
	addq	%rdx, %rax
	subq	$6, %rax
	movss	-288(%rbp,%rax,4), %xmm0
	mulss	%xmm2, %xmm0
	movl	-8(%rbp), %eax
	cltq
	leaq	0(,%rax,4), %rdx
	movl	-4(%rbp), %eax
	cltq
	addq	%rdx, %rax
	subq	$5, %rax
	addss	%xmm1, %xmm0
	movss	%xmm0, -416(%rbp,%rax,4)
	addl	$1, -12(%rbp)
	jmp	.L23
.L57:
	nop
	addl	$1, -8(%rbp)
	jmp	.L24
.L56:
	nop
	addl	$1, -4(%rbp)
	jmp	.L25
.L55:
	nop
	leaq	.LC1(%rip), %rax
	movq	%rax, -1016(%rbp)
	movl	$48, -1008(%rbp)
	movl	$128, -1024(%rbp)
	movl	$6, -1020(%rbp)
	leaq	-1024(%rbp), %rax
	movq	%rax, %rdi
	call	_gfortran_st_write@PLT
	leaq	-1024(%rbp), %rax
	movl	$9, %edx
	leaq	.LC2(%rip), %rcx
	movq	%rcx, %rsi
	movq	%rax, %rdi
	call	_gfortran_transfer_character_write@PLT
	leaq	-1024(%rbp), %rax
	movq	%rax, %rdi
	call	_gfortran_st_write_done@PLT
	movl	$1, -4(%rbp)
.L30:
	cmpl	$4, -4(%rbp)
	setg	%al
	movzbl	%al, %eax
	testl	%eax, %eax
	jne	.L58
	leaq	.LC1(%rip), %rax
	movq	%rax, -1016(%rbp)
	movl	$50, -1008(%rbp)
	movl	$128, -1024(%rbp)
	movl	$6, -1020(%rbp)
	leaq	-1024(%rbp), %rax
	movq	%rax, %rdi
	call	_gfortran_st_write@PLT
	movl	$1, -8(%rbp)
.L29:
	movl	-1024(%rbp), %eax
	andl	$3, %eax
	andl	$1, %eax
	testb	%al, %al
	jne	.L59
	cmpl	$3, -8(%rbp)
	setg	%al
	movzbl	%al, %eax
	testl	%eax, %eax
	jne	.L60
	movl	-8(%rbp), %eax
	cltq
	leaq	0(,%rax,4), %rdx
	movl	-4(%rbp), %eax
	cltq
	addq	%rdx, %rax
	subq	$5, %rax
	leaq	-64(%rbp), %rdx
	salq	$2, %rax
	leaq	(%rdx,%rax), %rcx
	leaq	-1024(%rbp), %rax
	movl	$4, %edx
	movq	%rcx, %rsi
	movq	%rax, %rdi
	call	_gfortran_transfer_real_write@PLT
	addl	$1, -8(%rbp)
	jmp	.L29
.L59:
	nop
	jmp	.L28
.L60:
	nop
.L28:
	leaq	-1024(%rbp), %rax
	movq	%rax, %rdi
	call	_gfortran_st_write_done@PLT
	addl	$1, -4(%rbp)
	jmp	.L30
.L58:
	nop
	leaq	.LC1(%rip), %rax
	movq	%rax, -1016(%rbp)
	movl	$52, -1008(%rbp)
	movl	$128, -1024(%rbp)
	movl	$6, -1020(%rbp)
	leaq	-1024(%rbp), %rax
	movq	%rax, %rdi
	call	_gfortran_st_write@PLT
	leaq	-1024(%rbp), %rax
	movq	%rax, %rdi
	call	_gfortran_st_write_done@PLT
	leaq	.LC1(%rip), %rax
	movq	%rax, -1016(%rbp)
	movl	$54, -1008(%rbp)
	movl	$128, -1024(%rbp)
	movl	$6, -1020(%rbp)
	leaq	-1024(%rbp), %rax
	movq	%rax, %rdi
	call	_gfortran_st_write@PLT
	leaq	-1024(%rbp), %rax
	movl	$9, %edx
	leaq	.LC3(%rip), %rcx
	movq	%rcx, %rsi
	movq	%rax, %rdi
	call	_gfortran_transfer_character_write@PLT
	leaq	-1024(%rbp), %rax
	movq	%rax, %rdi
	call	_gfortran_st_write_done@PLT
	movl	$1, -4(%rbp)
.L35:
	cmpl	$3, -4(%rbp)
	setg	%al
	movzbl	%al, %eax
	testl	%eax, %eax
	jne	.L61
	leaq	.LC1(%rip), %rax
	movq	%rax, -1016(%rbp)
	movl	$56, -1008(%rbp)
	movl	$128, -1024(%rbp)
	movl	$6, -1020(%rbp)
	leaq	-1024(%rbp), %rax
	movq	%rax, %rdi
	call	_gfortran_st_write@PLT
	movl	$1, -8(%rbp)
.L34:
	movl	-1024(%rbp), %eax
	andl	$3, %eax
	andl	$1, %eax
	testb	%al, %al
	jne	.L62
	cmpl	$5, -8(%rbp)
	setg	%al
	movzbl	%al, %eax
	testl	%eax, %eax
	jne	.L63
	movl	-8(%rbp), %eax
	movslq	%eax, %rdx
	movq	%rdx, %rax
	addq	%rax, %rax
	addq	%rax, %rdx
	movl	-4(%rbp), %eax
	cltq
	addq	%rdx, %rax
	subq	$4, %rax
	leaq	-128(%rbp), %rdx
	salq	$2, %rax
	leaq	(%rdx,%rax), %rcx
	leaq	-1024(%rbp), %rax
	movl	$4, %edx
	movq	%rcx, %rsi
	movq	%rax, %rdi
	call	_gfortran_transfer_real_write@PLT
	addl	$1, -8(%rbp)
	jmp	.L34
.L62:
	nop
	jmp	.L33
.L63:
	nop
.L33:
	leaq	-1024(%rbp), %rax
	movq	%rax, %rdi
	call	_gfortran_st_write_done@PLT
	addl	$1, -4(%rbp)
	jmp	.L35
.L61:
	nop
	leaq	.LC1(%rip), %rax
	movq	%rax, -1016(%rbp)
	movl	$58, -1008(%rbp)
	movl	$128, -1024(%rbp)
	movl	$6, -1020(%rbp)
	leaq	-1024(%rbp), %rax
	movq	%rax, %rdi
	call	_gfortran_st_write@PLT
	leaq	-1024(%rbp), %rax
	movq	%rax, %rdi
	call	_gfortran_st_write_done@PLT
	leaq	.LC1(%rip), %rax
	movq	%rax, -1016(%rbp)
	movl	$60, -1008(%rbp)
	movl	$128, -1024(%rbp)
	movl	$6, -1020(%rbp)
	leaq	-1024(%rbp), %rax
	movq	%rax, %rdi
	call	_gfortran_st_write@PLT
	leaq	-1024(%rbp), %rax
	movl	$9, %edx
	leaq	.LC4(%rip), %rcx
	movq	%rcx, %rsi
	movq	%rax, %rdi
	call	_gfortran_transfer_character_write@PLT
	leaq	-1024(%rbp), %rax
	movq	%rax, %rdi
	call	_gfortran_st_write_done@PLT
	movl	$1, -4(%rbp)
.L40:
	cmpl	$5, -4(%rbp)
	setg	%al
	movzbl	%al, %eax
	testl	%eax, %eax
	jne	.L64
	leaq	.LC1(%rip), %rax
	movq	%rax, -1016(%rbp)
	movl	$62, -1008(%rbp)
	movl	$128, -1024(%rbp)
	movl	$6, -1020(%rbp)
	leaq	-1024(%rbp), %rax
	movq	%rax, %rdi
	call	_gfortran_st_write@PLT
	movl	$1, -8(%rbp)
.L39:
	movl	-1024(%rbp), %eax
	andl	$3, %eax
	andl	$1, %eax
	testb	%al, %al
	jne	.L65
	cmpl	$8, -8(%rbp)
	setg	%al
	movzbl	%al, %eax
	testl	%eax, %eax
	jne	.L66
	movl	-8(%rbp), %eax
	movslq	%eax, %rdx
	movq	%rdx, %rax
	salq	$2, %rax
	addq	%rax, %rdx
	movl	-4(%rbp), %eax
	cltq
	addq	%rdx, %rax
	subq	$6, %rax
	leaq	-288(%rbp), %rdx
	salq	$2, %rax
	leaq	(%rdx,%rax), %rcx
	leaq	-1024(%rbp), %rax
	movl	$4, %edx
	movq	%rcx, %rsi
	movq	%rax, %rdi
	call	_gfortran_transfer_real_write@PLT
	addl	$1, -8(%rbp)
	jmp	.L39
.L65:
	nop
	jmp	.L38
.L66:
	nop
.L38:
	leaq	-1024(%rbp), %rax
	movq	%rax, %rdi
	call	_gfortran_st_write_done@PLT
	addl	$1, -4(%rbp)
	jmp	.L40
.L64:
	nop
	leaq	.LC1(%rip), %rax
	movq	%rax, -1016(%rbp)
	movl	$64, -1008(%rbp)
	movl	$128, -1024(%rbp)
	movl	$6, -1020(%rbp)
	leaq	-1024(%rbp), %rax
	movq	%rax, %rdi
	call	_gfortran_st_write@PLT
	leaq	-1024(%rbp), %rax
	movq	%rax, %rdi
	call	_gfortran_st_write_done@PLT
	leaq	.LC1(%rip), %rax
	movq	%rax, -1016(%rbp)
	movl	$66, -1008(%rbp)
	movl	$128, -1024(%rbp)
	movl	$6, -1020(%rbp)
	leaq	-1024(%rbp), %rax
	movq	%rax, %rdi
	call	_gfortran_st_write@PLT
	leaq	-1024(%rbp), %rax
	movl	$9, %edx
	leaq	.LC5(%rip), %rcx
	movq	%rcx, %rsi
	movq	%rax, %rdi
	call	_gfortran_transfer_character_write@PLT
	leaq	-1024(%rbp), %rax
	movq	%rax, %rdi
	call	_gfortran_st_write_done@PLT
	movl	$1, -4(%rbp)
.L45:
	cmpl	$4, -4(%rbp)
	setg	%al
	movzbl	%al, %eax
	testl	%eax, %eax
	jne	.L67
	leaq	.LC1(%rip), %rax
	movq	%rax, -1016(%rbp)
	movl	$68, -1008(%rbp)
	movl	$128, -1024(%rbp)
	movl	$6, -1020(%rbp)
	leaq	-1024(%rbp), %rax
	movq	%rax, %rdi
	call	_gfortran_st_write@PLT
	movl	$1, -8(%rbp)
.L44:
	movl	-1024(%rbp), %eax
	andl	$3, %eax
	andl	$1, %eax
	testb	%al, %al
	jne	.L68
	cmpl	$8, -8(%rbp)
	setg	%al
	movzbl	%al, %eax
	testl	%eax, %eax
	jne	.L69
	movl	-8(%rbp), %eax
	cltq
	leaq	0(,%rax,4), %rdx
	movl	-4(%rbp), %eax
	cltq
	addq	%rdx, %rax
	subq	$5, %rax
	leaq	-416(%rbp), %rdx
	salq	$2, %rax
	leaq	(%rdx,%rax), %rcx
	leaq	-1024(%rbp), %rax
	movl	$4, %edx
	movq	%rcx, %rsi
	movq	%rax, %rdi
	call	_gfortran_transfer_real_write@PLT
	addl	$1, -8(%rbp)
	jmp	.L44
.L68:
	nop
	jmp	.L43
.L69:
	nop
.L43:
	leaq	-1024(%rbp), %rax
	movq	%rax, %rdi
	call	_gfortran_st_write_done@PLT
	addl	$1, -4(%rbp)
	jmp	.L45
.L67:
	nop
	nop
	leave
	.cfi_def_cfa 7, 8
	ret
	.cfi_endproc
.LFE0:
	.size	MAIN__, .-MAIN__
	.globl	main
	.type	main, @function
main:
.LFB1:
	.cfi_startproc
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register 6
	subq	$16, %rsp
	movl	%edi, -4(%rbp)
	movq	%rsi, -16(%rbp)
	movq	-16(%rbp), %rdx
	movl	-4(%rbp), %eax
	movq	%rdx, %rsi
	movl	%eax, %edi
	call	_gfortran_set_args@PLT
	leaq	options.11.0(%rip), %rax
	movq	%rax, %rsi
	movl	$7, %edi
	call	_gfortran_set_options@PLT
	call	MAIN__
	movl	$0, %eax
	leave
	.cfi_def_cfa 7, 8
	ret
	.cfi_endproc
.LFE1:
	.size	main, .-main
	.section	.rodata
	.align 16
	.type	options.11.0, @object
	.size	options.11.0, 28
options.11.0:
	.long	2116
	.long	4095
	.long	0
	.long	1
	.long	1
	.long	0
	.long	31
	.ident	"GCC: (Ubuntu 11.4.0-1ubuntu1~22.04) 11.4.0"
	.section	.note.GNU-stack,"",@progbits
