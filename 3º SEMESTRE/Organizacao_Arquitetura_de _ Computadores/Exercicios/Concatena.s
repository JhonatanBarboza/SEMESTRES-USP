#  Codifique em Assembly RISC-V um programa que implemente a funcionalidade da função strcat() (concatenar strings)

 	.data
 	.align 0
 str1:	.asciz "Digite a string A: "
 str2:	.asciz "Digite a string B: "
 str3:	.asciz "A string cocatenada é: "

 buffer_A: 	.space 50
 buffer_B:	.space 50
 buffer_res:	.space 100
 
   	.text
   	.align 2
   	.globl main

main:
	#imprimir mensagem de entrada A
	li a7, 4
	la a0, str1
	ecall
	
	# ler a string A do usuario 
	li a7, 8
	la a0, buffer_A
	li a1, 50
	ecall	
	
	# salvando a str A
	mv s0, a0
	
	#imprimir mensagem de entrada B
	li a7, 4
	la a0, str2
	ecall
	
	# ler a string B do usuario 
	li a7, 8
	la a0, buffer_B
	li a1, 50
	ecall	
	
	# salvando a strB
	mv s1, a0
	
	# contar elementos de str A
	mv a0, s0
	jal conta_str
	mv s2, a1	# salva o tamanho da strA em s2 2
	
	# contar elementos de str B
	mv a0, s1
	jal conta_str
	mv s3, a1	# salva o tamanho da strA em s3
	
	
	add t0, s2, s3		# tamanho total da string concatenada
	la t1, buffer_res	# aponta para srt resultado
	li t6, 1 		#contador 
	
	# Atrinbui as duas string A a string c
loop_A:
	lb t2, 0(s0)		# elementos da primeira string
	sb t2, 0(t1)		# salva em buffer_res
	
	beq t6, s2, end_loop_A	# sai do loop se t6 >= s2 
	addi s0, s0, 1		# proximo caractere 
	addi t1, t1, 1		# incrementa a pocição para salvar 
	addi t6, t6, 1		# contador ++
	j loop_A
	
end_loop_A:
	# atribuir a string B
	
	addi t1, t1, 1		# proximapossição para salvar 
	addi t6, t6, 1		# incrementa o contador 
	
loop_B:
	lb t2, 0(s1)		# elementos da string B
	sb t2, 0(t1)		# salva elemento na str concatenada
	
	beq t6, t0, end_loop_B	# percorre ate o fim do tamanho total
	addi s1, s1, 1		# proximo elemento
	addi t6, t6, 1		# contador++
	addi t1, t1, 1		# proximo elemento a ser salvo 
	j loop_B
	
end_loop_B:

	# imprimir string concatenada
	addi t1, t1, 1
	sb zero, 0(t1)		# coloca o nuul no fim da string
	
	# imprimir string resultado
	li a7, 4
	la a0, buffer_res
	ecall
	
	# encerra o programa 
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
	      
	        
	          
	            
	              
	                
	                  
	                      