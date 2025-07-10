	## Codifique em Assembly RISC-V um programa que soma os valores de um vetor de inteiros de tamanho definido 
	## no próprio programa. Os dados do vetor também podem já estar definidos no código.
	
	.data
	
	.align 2
vetor:	.word  1, 2, 3, 4, 5		# vetor para somar os caracteres 
tam:	.word  5

	.text
	.align 2
	.globl main 
	
main: 	## iniciar os registradores 
	
	la t0, vetor 		## aponta para o vetor
	lw t1, tam		## guarda o tamanho do vetor 
	li t2, 0		## contador: inicia em zero 
	li t3, 0		## resultado 
	
loop: 	## loop para somar os elementos do registrador 

	bge t2, t1, end_loop		## t2 >= t1 vai para end_loop
	lw t4, 0(t0)			## carrega o primeiro numero do vetor
	add t3, t3, t4			## soma os elementos do vetor 
	addi t0, t0, 4			## proxomo valor do vetor 
	addi t2, t2, 1			## incrementa o vetor 
	j loop
	
end_loop: 	## imprimir o resultado e encerar o programa 
	
	mv a0, t3		## salvar o resultado 
	li a7, 1		## imprime o resultado 
	ecall
	
	li a7, 10		## finaliza o programa
	ecall 

	