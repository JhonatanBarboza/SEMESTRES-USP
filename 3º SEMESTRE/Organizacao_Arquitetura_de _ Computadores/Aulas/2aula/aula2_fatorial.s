.data
    .align 0 
msf_leitura:    .asciz "Digite um numero: "  # Mensagem para leitura do input
msg_fat:        .asciz "O fatorial de "     # Mensagem para o fatorial
msg_eh:         .asciz " eh "               # Mensagem "eh"
    .text 
    .align 2
    .globl main

main: 
    # Imprimir a mensagem para leitura do input
    li a7, 4              # Código para imprimir uma string
    la a0, msf_leitura     # Carrega o endereço da mensagem em a0
    ecall                  # Faz a chamada do sistema para imprimir a mensagem

    # Ler o input do usuário
    li a7, 5               # Código para ler um inteiro (input)
    ecall                  # Faz a chamada do sistema para ler o input
    mv t0, a0              # t0 recebe o valor do input (n)
    li t1, 1               # t1 é o resultado (fatorial)

entra_loop:
    beq t0, zero, final    # Se o contador (t0) for zero, vá para 'final'
    mul t1, t1, t0         # Multiplica o resultado (t1) pelo contador (t0)
    addi t0, t0, -1        # Decrementa o contador (t0) em 1
    j entra_loop           # Volta para o início do loop

final:
    # Imprimir a mensagem "O fatorial de"
    li a7, 4               # Código para imprimir uma string
    la a0, msg_fat         # Carrega o endereço da mensagem em a0
    ecall                  # Faz a chamada do sistema para imprimir a mensagem

    # Imprimir o número de entrada (n)
    li a7, 1               # Código para imprimir um inteiro
    mv a0, t0              # Move o valor de entrada (t0) para a0
    ecall                  # Faz a chamada do sistema para imprimir o número

    # Imprimir a mensagem "eh"
    li a7, 4               # Código para imprimir uma string
    la a0, msg_eh          # Carrega o endereço da mensagem em a0
    ecall                  # Faz a chamada do sistema para imprimir a mensagem

    # Imprimir o resultado do fatorial
    li a7, 1               # Código para imprimir um inteiro
    mv a0, t1              # Move o resultado (t1) para a0
    ecall                  # Faz a chamada do sistema para imprimir o resultado

    # Finalizar o programa
    li a7, 10              # Código para terminar o programa
    ecall                  # Faz a chamada do sistema para terminar o programa