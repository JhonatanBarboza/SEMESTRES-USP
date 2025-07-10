.data
    # Variáveis
    prompt:     .asciz "Digite o tamanho do vetor (1-20): "
    elem_prompt:.asciz "Digite o elemento ["
    elem_prompt2:.asciz "]: "
    vetor:      .word 0:20       # Vetor com espaço para 20 elementos
    tamanho:    .word 0          # Tamanho real do vetor
    newline:    .asciz "\n"

.text
    .align 2
    .globl main

main:
    # Solicita tamanho do vetor
    li a7, 4
    la a0, prompt
    ecall
    
    li a7, 5           # Lê inteiro (tamanho)
    ecall
    mv s1, a0          # s1 = tamanho
    sw s1, tamanho, t0 # Armazena na memória
    
    # Verifica tamanho válido (1-20)
    li t0, 1
    blt s1, t0, main   # Se < 1, repete
    li t0, 20
    bgt s1, t0, main   # Se > 20, repete
    
    # Prepara para ler elementos
    la s0, vetor       # s0 = endereço do vetor
    li t0, 0           # t0 = contador

read_loop:
    bge t0, s1, sort   # Se leu todos, vai para ordenação
    
    # Mostra prompt do elemento
    li a7, 4
    la a0, elem_prompt
    ecall
    
    li a7, 1
    mv a0, t0
    ecall              # Imprime índice
    
    li a7, 4
    la a0, elem_prompt2
    ecall
    
    # Lê elemento
    li a7, 5
    ecall
    sw a0, 0(s0)       # Armazena no vetor
    
    addi s0, s0, 4     # Avança para próximo elemento
    addi t0, t0, 1     # Incrementa contador
    j read_loop

sort:
    # Inicializa ordenação
    la s0, vetor       # Recarrega endereço do vetor
    addi s1, s1, -2    # s1 = tamanho-2
    li t0, -1          # t0 = contador externo (i)

loop_externo:
    bgt t0, s1, print_result
    
    addi t0, t0, 1
    li t1, 0           # t1 = contador interno (j)

loop_interno:
    bgt t1, s1, loop_externo
    
    slli t2, t1, 2
    add t3, s0, t2
    lw t4, 0(t3)
    lw t5, 4(t3)
    
    addi t1, t1, 1
    
    ble t4, t5, loop_interno
    
    # Swap
    sw t5, 0(t3)
    sw t4, 4(t3)
    j loop_interno

print_result:
    # Imprime vetor ordenado
    li a7, 4
    la a0, newline
    ecall
    
    la s0, vetor       # Recarrega endereço
    lw s1, tamanho     # Recarrega tamanho
    li t0, 0           # Contador

print_loop:
    bge t0, s1, exit
    
    li a7, 1
    lw a0, 0(s0)
    ecall
    
    # Imprime espaço
    li a7, 11
    li a0, ' '
    ecall
    
    addi s0, s0, 4
    addi t0, t0, 1
    j print_loop

exit:
    li a7, 10
    ecall