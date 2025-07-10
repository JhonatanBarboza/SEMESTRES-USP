	.data
	.align 2
	
str1:	.asciz "digite um numero\n"
str2:	.asciz "o fatorial calculado é: "

	.text
	.align 2
	.globl main
	
main: 
	## imprimir str1
	li a7, 4
	la a0, str1
	ecall
	
	## ler o valor digitado
	li a7, 5		## o valor é guardado em a0
	ecall
	
	add s0, zero, a0	## salva o valor em s0
	
	## chamar a função
	jal fatorial
	
	add s1, zero, a1
	
	li a7, 4		## imprime mensagem de saída
	la a0, str2
	ecall
		
	li a7, 1		## imprimir o valor
	mv a0, s1
	ecall
	
	li a7, 10		## finalizar o programa 
	ecall
	
	
## função fatorial
## a0 nº a ser calculado 
## a1 retorno da função
fatorial: 
	addi a1, zero, 1	## inicia o retorno com 1
	mv t0, a0		## contador 

loop:
	beqz t0, end_loop	## se contador == 0, sai
	mul a1, a1, t0		## multiplica o resultado pelo contador
	addi t0, t0, -1	## decrementa o contador
	j loop
	
end_loop:
	ret
