	.text
	.extern alloc
	.extern make_closure
	.extern make_recursive_closure
	.globl entry
	.type entry, @function
.L0:
	pushq %rbp
	movq %rsp,%rbp
	subq $16,%rsp
	movq (%rsi),%rax
	movq %rax,-16(%rbp)
	movq %rdi,-8(%rbp)
	movq -8(%rbp),%rax
	pushq %rax
	movq $0,%rax
	movq %rax,%rbx
	popq %rax
	cmpq %rbx,%rax
	jne .L3
	movq $1,%rax
	jmp .L4
.L3:
	movq $0,%rax
.L4:
	cmpq $1,%rax
	jne .L1
	movq $0,%rax
	jmp .L2
.L1:
	movq -8(%rbp),%rax
	pushq %rax
	movq $1,%rax
	movq %rax,%rbx
	popq %rax
	cmpq %rbx,%rax
	jne .L7
	movq $1,%rax
	jmp .L8
.L7:
	movq $0,%rax
.L8:
	cmpq $1,%rax
	jne .L5
	movq $1,%rax
	jmp .L6
.L5:
	movq -16(%rbp),%rax
	pushq %rax
	movq -8(%rbp),%rax
	pushq %rax
	movq $1,%rax
	movq %rax,%rbx
	popq %rax
	subq %rbx,%rax
	movq %rax,%rdi
	popq %rax
	movq 8(%rax),%rsi
	movq (%rax),%rax
	call *%rax
	pushq %rax
	movq -16(%rbp),%rax
	pushq %rax
	movq -8(%rbp),%rax
	pushq %rax
	movq $2,%rax
	movq %rax,%rbx
	popq %rax
	subq %rbx,%rax
	movq %rax,%rdi
	popq %rax
	movq 8(%rax),%rsi
	movq (%rax),%rax
	call *%rax
	movq %rax,%rbx
	popq %rax
	addq %rbx,%rax
.L6:
.L2:
	movq %rbp,%rsp
	popq %rbp
	ret
entry:
	pushq %rbp
	movq %rsp,%rbp
	subq $8,%rsp
	leaq .L0(%rip),%rdi
	movq $0,%rsi
	xorq %rax,%rax
	call make_recursive_closure
	movq %rax,-8(%rbp)
	movq -8(%rbp),%rax
	pushq %rax
	movq $10,%rax
	movq %rax,%rdi
	popq %rax
	movq 8(%rax),%rsi
	movq (%rax),%rax
	call *%rax
	movq %rbp,%rsp
	popq %rbp
	ret
