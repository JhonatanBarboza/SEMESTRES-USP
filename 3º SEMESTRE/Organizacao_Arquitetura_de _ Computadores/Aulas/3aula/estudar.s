	# INICIALIZA VARIAVEIS 
	
	#inicializar o programa
	.data
	# Como irei declarar uma string presiso alinhar bit a bit 
	.align 0
	# Criar minha string
str_src: .asciz "oi mae!!" 
	
	# declarar uma palavra multiplode quatro 
	.align 2
ponteiro: .word

	# INICIALIZAR O PROGRAMA 
	.text
	.align 2 # pois o programa tem 32 bits
	
	.globl main

main:
	# PRIMEIRO CONTAR QUANTOS BITS TEM A STRING ATE O /0 
	#assim teremos o tamanho para posteriormente alocar na heap
	# pence em um for primeiro temos que apontar para o inicio da string e inicializar o contador 
	la s0, str_src		# aponta para o iniciode str_src
	li t1, 0 		#contador t1 tera o tamanho da string
	
	#iniciar o loop
loopContStr:
	la t0, 0(s0) 		# vai percorrer a string 
	addi s0, s0, 1  	# avança o ponteiro
	addi t1, t1, 1		# increnta o contador 
	bne t0, zero, loopContStr 	# enquanto t0!=/0 retorna o loop
	

	

		
	
	