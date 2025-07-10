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
	
	mv s0, a0		## salva o valor em s0
	
	## chamar a função
	jal fatorial
	
	mv s1, a1		## salva o resultado em s1
	
	li a7, 4		## imprime mensagem de saída
	la a0, str2
	ecall
		
	li a7, 1		## imprimir o valor
	mv a0, s1
	ecall
	
	li a7, 10		## finalizar o programa 
	ecall
	
	
## função fatorial recursiva
## Entrada: a0 (número a ser calculado)
## Saída: a1 (resultado do fatorial)
fatorial: 
	## empilhar ra e a0
	addi sp, sp, -8		## Reserva espaço na pilha
	sw ra, 4(sp)		## Salva ra (endereço de retorno)
	sw a0, 0(sp)		## Salva a0 (número)
	
	## Caso base: se n == 0, retorna 1
	beqz a0, caso_base
	
	## Chamada recursiva fatorial(n-1)
	addi a0, a0, -1
	jal fatorial
	
	## Multiplica a1 (fatorial(n-1)) por n
	lw a0, 0(sp)		## Recupera n original da pilha
	mul a1, a1, a0		## a1 = n * fatorial(n-1)
	
	j retfat		## Vai para a parte de retorno

caso_base:
	li a1, 1		## Retorna 1 para fatorial(0)
	j retfat

retfat:
	lw ra, 4(sp)		## Recupera ra (endereço de retorno)
	addi sp, sp, 8		## Libera espaço na pilha
	ret			## Retorna para a chamada anterior
