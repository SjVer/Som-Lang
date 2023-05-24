	.text
	.file	"test.som"
	.globl	"text/2"
	.p2align	4, 0x90
	.type	"text/2",@function
"text/2":
	.cfi_startproc
	pushq	%rax
	.cfi_def_cfa_offset 16
	leaq	.Lstr(%rip), %rdi
	callq	som_str_make@PLT
	popq	%rcx
	.cfi_def_cfa_offset 8
	retq
.Lfunc_end0:
	.size	"text/2", .Lfunc_end0-"text/2"
	.cfi_endproc

	.globl	"main/3"
	.p2align	4, 0x90
	.type	"main/3",@function
"main/3":
	.cfi_startproc
	pushq	%rax
	.cfi_def_cfa_offset 16
	callq	"text/2"@PLT
	movq	%rax, %rdi
	callq	som_str_length@PLT
	popq	%rcx
	.cfi_def_cfa_offset 8
	retq
.Lfunc_end1:
	.size	"main/3", .Lfunc_end1-"main/3"
	.cfi_endproc

	.type	.Lstr,@object
	.section	.rodata.str1.1,"aMS",@progbits,1
.Lstr:
	.asciz	"Hello, World!"
	.size	.Lstr, 14

	.type	som_entrypoint,@object
	.data
	.globl	som_entrypoint
	.p2align	3
som_entrypoint:
	.quad	"main/3"
	.size	som_entrypoint, 8

	.section	".note.GNU-stack","",@progbits
