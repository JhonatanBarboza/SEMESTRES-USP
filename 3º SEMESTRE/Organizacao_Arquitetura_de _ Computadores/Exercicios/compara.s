# Codifique em Assembly RISC-V um programa que implemente a funcionalidade da função strcmp().

	.data
	.align 0

str1: 		.asciz "Digite a primeira string: "
str2: 		.asciz "Digite a segunda string: "
iguais: 	.asciz 	"SÃO IGUAIS"
diferentes:	.asciz "DIFERENTES" 
	
buffer1:	.space 50
buffer2:	.space 50

	.text
	.align 2
	.globl main
main: 

	# imprimir str1
	li a7, 4
	la a0, str1
	ecall
	
	# ler 1º string
	li a7, 8
	la a0, buffer1
	li a1, 50
	ecall
	
	# conta os caracteres
	jal conta_str 
	mv s0, a1
	
	# imprimir str2
	li a7, 4
	la a0, str2
	ecall
	
	# ler 2º string
	li a7, 8
	la a0, buffer2
	li a1, 50
	ecall
	
	# conta os caracteres
	jal conta_str
	mv s1, a1
	
	bne s0, s1, diferente
	
	la t0, buffer1
	la t1, buffer2
	
loop: 
	# comparar
	lb t2, 0(t0)
	lb t3, 0(t1)
	
	bne t2, t3, diferente
	addi s0, s0, -1
	addi t0, t0, 1
	addi t1, t1, 1
	beqz s0, igual
	j loop
	
	
	
igual: 
	# imprime igual
	li a7, 4 
	la a0, iguais
	ecall
	
	li a7, 10
	ecall
	
	
	
diferente:
	# imprime diferente
	li a7, 4 
	la a0, diferentes
	ecall
	
	li a7, 10
	ecall
	
	
	
# conta_str conta quantos elementos tem emuma string
# recebe a0 como o ponteiro para inicio da string 
# retorna a1 com o tamanho da string
	
conta_str:
	mv t2, a0		# inicio da string
	li t0, 0		# contador 
	
loop_cont: 
	lb t1, 0(t2)		# percorre os bits
	beqz t1, end_loop_cont	# sai do loop no fim da string
	addi t2, t2, 1		# proximo bit
	addi t0, t0, 1		# contador ++
	j loop_cont

end_loop_cont:
	
	addi t0, t0, -1		# apoontar para o ultimo caractere valido 
	mv a1, t0		# salva no registrador de retorno
	ret 			# retorna a função
	
	
	
	