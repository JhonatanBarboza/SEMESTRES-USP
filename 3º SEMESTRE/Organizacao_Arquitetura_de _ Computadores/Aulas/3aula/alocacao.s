# str_cpy() dinâmica -- ver anotações tablet Aula07
		.data # Dados do programa
		.align 0
str_src:	.asciz "Oi mae!!" # 9 bytes

		.align 2    
ponteiro:	.word # Ponteiro que vai apontar para um espaço na heap

		.text # Código do programa
		.align 2 # Instruções de 32 bits
		.globl main # Ponto de entrada do código
		
main:		# Main
		la s0, str_src # Guardando em s0 o endereço do início de str_src
		li t1, 0 # Contador inicializado com 0
		
loopContChar:	# Conta o número de caracteres da string
		lb t0, 0(s0) # Armazenando em t0 o char lido de s0
		addi s0, s0, 1 # Avançando o ponteiro da string
		addi t1, t1, 1 # Incrementando o contador
		bne t0, zero, loopContChar #  # Voltando para o inicio do loop caso não tenhamos chegado no \0
----------------------------------------------------------------------
heap:		# Alocação dinâmica na heap
		mv a0, t1 # Passando como argumento da função (espaço a ser alocado) a quantidade de caracteres da string (incluindo \0)
		li a7, 9 # Instrução para alocar espaço na memória
		ecall # Chamada do programa - a0 agora guarda o endereço do 1º byte endereçado
		
		# Fazendo ponteiro apontar para a heap
		la t2, ponteiro # Guardando em t2 o endereço do ponteiro
		sw a0, 0(t2) # Colocando o conteúdo de a0 na posição de memória apontada por t2
		
copia:		# Preparar para copiar
		la s0, str_src # Colocando em s0 o endereço da string str_src
		lw s1, 0(t2) # Colocando em s1 o endereço do espaço na heap
		
loopCopia:	# Copiando string para a heap
		lb t0, 0(s0) # Carregando em t0 o caractere da string
		sb t0, 0(s1) # Escrevendo na heap o caractere guardado em t0
		beq t0, zero, fim # Vai para o fim do programa caso chegue no \0
		addi, s0, s0, 1 # Avançando para o próximo caractere da string
		addi s1, s1, 1 # Avançando para o próximo espaço na heap
		j loopCopia # Volta para o início do loop
		
fim:		# Imprime a string na heap e encerra o programa
		lw s1, 0(t2) # Colocando em s1 o endereço do espaço na heap
		mv a0, s1 # Colocando em s0 o endereço do espaço na heap
		li a7, 4 # Instrução para imprimir string
		ecall # Chamada do programa
		
		li a7, 10 # Instrução para encerrar o programa
		ecall