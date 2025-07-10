# Implementação de uma calculadora sequencial em Assembly RISC-V
# Operações suportadas: + (soma), - (subtração), * (multiplicação), / (divisão)
# Comandos especiais: u (undo - desfazer), f (finalizar)

    .data
    .align 0
    
num:        .asciz "Digite o número: "          				# Prompt para entrada de número
ope:        .asciz "Digite o operador: "        				# Prompt para entrada de operador
res:        .asciz "O resultado é: "            				# Mensagem de resultado
msg_undo:   .asciz "O resultado anterior é: "   				# Mensagem para operação undo
linha:      .asciz "\n"                         				# Quebra de linha
ope_inv:    .asciz "\n *** Operador inválido *** \n"                		# Erro de operador inválido
res_ant:    .asciz "\n *** Não há resultado anterior *** \n"         		# Erro quando não há undo possível
div_zer:    .asciz "\n *** Não é possível dividir por zero *** \n"  		# Erro de divisão por zero

    .text
    .align 2
    .globl main

# Função principal - Controla o fluxo da calculadora
main: 
    	# Inicializa a pilha encadeada (s10 é ponteiro para o nó do topo da pilha)
    	li s10, 0 		# s10 = NULL
    	# Lê o primeiro número do usuário
    	jal le_num
    	mv s0, a0        	# Armazena o primeiro número em s0 para iniciar o loop
    
# Loop para manter a execução do programa
loop_principal:

    	# Lê o operador do usuário
    	jal le_ope
    	mv s1, a0        	# Armazena o operador em s1
    	mv a1, s0        	# Prepara o último resultado (s0) como primeiro operando (a1)
    
    	# Verifica qual operação foi solicitada (switch case)
    	li t0, 43         	# '+' em ASCII
    	beq s1, t0, soma  	# Se for '+', vai para soma
    	li t0, 45         	# '-' em ASCII
    	beq s1, t0, subt  	# Se for '-', vai para subtração
    	li t0, 42 		# '*' em ASCII
    	beq s1, t0, mult	# Se for '*' vai para multiplicação
    	li t0, 47		# '/' em ASCII
    	beq s1, t0, divi	# Se for '/' vai para divisão
    
    	li t0, 102 		# 'f' em ASCII
    	beq s1, t0, finalizar	# Se for finalizar o programa 
    	li t0, 117		# 'u' em ASCII
    	beq s1, t0, undo	# Se for retornar à última operação
    
   	# Tratamento para operador inválido
    	li a7, 4
    	la a0, ope_inv
    	ecall
    	
    	# Imprime quebra de linha
    	li a7, 4
    	la a0, linha
    	ecall
    
    	j loop_principal

# Exibe o resultado da operação e volta para o loop
resultado: 

    	# Imprime mensagem de resultado
    	li a7, 4 
    	la a0, res
    	ecall
    
        # Imprime resultado
    	mv a0, s0
    	li a7, 1
    	ecall
    
    	# Imprime quebra de linha
    	li a7, 4
    	la a0, linha
    	ecall

    	j loop_principal
    

# Operação de soma
# Recebe em a1 um dos operandos da soma (s0 = a1 + a0)
soma:
    	jal le_num	 # faz a leitura do segundo operando, retornado em a0
    	add s0, a1, a0
    	j insere_no			

# Operação de subtração
# Recebe em a1 um dos operandos da subtração (s0 = a1 - a0)
subt:
    	jal le_num	 # faz a leitura do segundo operando, retornado em a0
    	sub s0, a1, a0 
    	j insere_no 

# Operação de multiplicação
# Recebe em a1 um dos operandos da multiplicação (s0 = a1 * a0)
mult:
    	jal le_num	 # faz a leitura do segundo operando, retornado em a0
    	mul s0, a1, a0
    	j insere_no

# Operação de divisão
# Recebe em a1 o dividendo  (s0 = a1 / a0)
divi:
    	jal le_num	 	# faz a leitura do segundo operando, retornado em a0
    	beqz a0, divi_zero  	# Se a0 == 0, pula para tratamento de erro
    	div a0, a1, a0
    	j insere_no
divi_zero:
    	# Imprime mensagem de erro de divisão por zero
    	li a7, 4
    	la a0, div_zer
    	ecall
    	
    	# Imprime quebra de linha
    	li a7, 4
    	la a0, linha
    	ecall
    
    	j loop_principal
    
    
# Operação undo - Retorna ao resultado anterior
undo:   
        # Se a pilha está vazia (s10 = NULL), vai para fim_undo    	
    	beqz s10, erro_undo
       
    	# Senão, atualiza s10 para receber o endereço do nó anterior (desempilha)
    	lw s10, 4(s10)
    
    	# Se o endereço do nó anterior é nulo (não há resultado anterior, s10 == NULL), vai para fim_undo
    	beqz s10, erro_undo
    
    	# Senão, atualiza s0 com a valor do nó anterior
    	lw s0, 0(s10)        
    
    	# Imprime mensagem de undo
    	li a7, 4
    	la a0, msg_undo
    	ecall    
    
    	# Imprime o valor anterior
    	li a7, 1
    	mv a0, s0
    	ecall
    
    	# Imprime quebra de linha
    	li a7, 4
    	la a0, linha
    	ecall
    
    	j loop_principal
    

# Tratamento quando não há undo possível
erro_undo:
    	li a7, 4		# Imprime a mensagem de erro 
    	la a0, res_ant 
    	ecall
    
    	li a7, 4		# Pula uma linha 
    	la a0, linha
    	ecall
    
    	# Reinicia a calculadora
    	j main 
    

# Insere um nó na lista encadeada (empilha)
# s0: valor a ser armazenado
# s10: endereço do nó do topo atual
insere_no:
    
    	# Aloca 8 bytes (4 para valor, 4 para ponteiro)
    	li a7, 9             # Syscall para alocação de memória
    	li a0, 8             # 8 bytes a serem alocados
    	ecall                # a0 recebe o endereço da memória alocada (1º byte)
    
    	# Armazena o valor e o ponteiro no novo nó
    	sw s0, 0(a0)         # Armazena o valor
    	sw s10, 4(a0)        # Armazena o ponteiro para o nó anterior (nó anterior = topo)
    
    	# Atualiza o ponteiro para o topo da lista
    	mv s10, a0           # s10 agora aponta para o novo nó
   
        j resultado

    
# Finaliza a execução do programa
finalizar: 
    	li a7, 10
    	ecall
    
# Lê um número do usuário
# Retorna em a0 o valor lido
le_num:
    	# Exibe prompt
    	li a7, 4
    	la a0, num
    	ecall
    
    	# Lê o número
    	li a7, 5
    	ecall
    	ret

# Lê um operador do usuário
# Retorna em a0 o operador lido
le_ope:
    	# Exibe prompt
    	li a7, 4
    	la a0, ope
    	ecall
    
    	# Lê o operador
    	li a7, 12
    	ecall
    	mv t6, a0            # Preserva o operador em t6
    
    	# Imprime quebra de linha
    	li a7, 4
    	la a0, linha
    	ecall
    
    	mv a0, t6            # Retorna o operador em a0
    	ret
