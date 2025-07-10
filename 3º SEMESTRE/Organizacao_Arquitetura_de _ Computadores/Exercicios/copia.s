# Codifique em Assembly RISC-V um programa que implemente a funcionalidade da função strcpy(). 

	.data
	.align 0
	
str1:	.asciz "Digite sua string: "
str2:	.asciz	"a string copiada é: "

buffer1: .space 50
buffer2: .space 50

	.text
	.align 2
	.globl main

main: 
	# imprimir mensagem
	li a7, 4
	la a0, str1
	ecall
	
	# ler string
	li a7, 8
	la a0, buffer1
	li a1, 50
	ecall
	
	# tamnho de strig
	jal conta_str
	
	# loop atribuir
	mv s0, a1
	la t0, buffer1
	la t1, buffer2
	
loop:
	lb t6, 0(t0)
	sb t6, 0(t1)
	
	beqz a1 end_loop
	addi t0, t0, 1
	addi t1, t1, 1
	addi a1, a1, -1
	j loop
	
end_loop:

	# imprime msg
	li a7, 4
	la a0, str2
	ecall

	# imprime 
	li a7, 4
	la a0, buffer2
	ecall
	
	# ensera
	li a7, 10
	ecall
	
	
	
# a0 string
# a1 tamanho 	
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
	      
	
	
	
	
	
	
	
	
	
	