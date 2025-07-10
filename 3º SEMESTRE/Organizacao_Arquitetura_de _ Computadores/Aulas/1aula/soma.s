.data
n1:     .word 2        # Primeiro número
n2:     .word 3        # Segundo número
res:    .word 0        # Variável para armazenar o resultado
msg:    .asciz "Resultado: "  # Mensagem para exibir

.text
.globl main

main:
    # Carregar os valores de n1 e n2 nos registradores
    lw t0, n1          # Carrega o valor de n1 em t0
    lw t1, n2          # Carrega o valor de n2 em t1

    # Somar os valores
    add t2, t0, t1     # t2 = t0 + t1

    # Armazenar o resultado na memória
    sw t2, res, t3     # Armazena o valor de t2 em res

    # Exibir a mensagem "Resultado: "
    li a7, 4           # Código da syscall para escrever uma string
    la a0, msg         # Carrega o endereço da mensagem em a0
    ecall              # Chama a syscall

    # Exibir o resultado (número)
    li a7, 1           # Código da syscall para escrever um inteiro
    lw a0, res         # Carrega o valor de res em a0
    ecall              # Chama a syscall

    # Encerrar o programa
    li a7, 10          # Código da syscall para sair
    ecall              # Chama a syscall