		# Dados do programa
		.data
		
		.align 1		    # Instruções de 2 bytes (2 bytes = 16 bits)
msg1:		.asciz "Informe o número para calcular o fatorial: "
msg2:		.asciz "Resultado: "
msg3:		.asciz "! eh "
		
		.text
		# Código do programa
		.align 2 		    # Instruções de 2² bytes (4 bytes = 32 bits)
		.globl main 		# Ponto de entrada do programa
		
		# Main função principal do programa
main:		
		# Imprimindo mensagem 1
		la a0, msg1		    # Carregando msg 1 em a0
		li a7, 4		    # Instrução de imprimir string (printf)
		ecall 			    # Chamada do programa
		
		# Lendo input do usuário		
		li a7, 5 		    # Lê input (n) do usuario (scanf)
		ecall 			    # Chamada do programa
		
		mv s0, a0 		    # Valor digitado pelo usuário guardado em s0
		mv a0, s0		    # garantir o paramentro da função em a0
		jal fatorial		# chamada da função para calcular o fatorial
		
fim:
		addi s1, a0, 0      # salva resultado da operação
		
		# Imprimindo os resultados
		la a0, msg2		    # Carregando msg 2 em a0
		li a7, 4 		    # Instrução de imprimir string
		ecall 			    # Chamada do programa
		
		addi a0, s0, 0 		# Carregando o número digitado pelo usuário em a0
		li a7, 1 		    # Instrução para imprimir um inteiro
		ecall 			    # Chamada do programa
		
		la a0, msg3 		# Carregando ms3 em a0
		li a7, 4 		    # Instrução de imprimir string
		ecall 			    # Chamada do programa
		
		addi a0, s1, 0 		# Colocando o resultado em a0
		li a7, 1 		    # Instrução para imprimir um inteiro
		ecall 			    # Chamada do programa
		
		li a7, 10 		    # Instrução para encerrar o programa
		ecall 			    # Chamada do programa


		
    		# Função para calcular o fatorial 
    		# Entrada: a0 - número para calcular fatorial
    		# Saída: a0 - resultado do fatorial

		# Função para calcular o fatorial 
fatorial:
		# Contador e resultado
		li t1, 1 		    # Resultado (0! = 1)
		addi t0, a0, 0 		# Contador decremental (i) (n -> 0)
		
		# Loop para calcular o fatorial
loop:
    		beq t0, zero, end_loop  # Se contador == 0, termina
   		mul t1, t1, t0              # t1 = t1 * t0 (res = res * i)
    		addi t0, t0, -1         # Decrementando contador (i--)
    		j loop

end_loop:
    		addi a0, t1, 0          # Move o resultado para a0 (registrador de retorno)
    		ret                     # Retorna para função que chamou 

		
