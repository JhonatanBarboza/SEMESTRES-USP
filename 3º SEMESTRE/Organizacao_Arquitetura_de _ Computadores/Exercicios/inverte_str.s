    .data
    .align 0
buffer:     .space 100        # reserva 100 bytes na memória para a string de entrada
buffer_res: .space 100        # reserva 100 bytes para a string invertida
str1:       .asciz "Digite sua frase: "
str2:       .asciz "Frase invertida: "

    .text
    .align 2
    .globl main

main:
    # Imprimir prompt para o usuário
    li a7, 4
    la a0, str1
    ecall

    # Ler string do usuário
    li a7, 8                # syscall para ler string (read string)
    la a0, buffer           # endereço do buffer
    li a1, 100              # tamanho máximo
    ecall

    # Encontrar o final da string (procurar pelo '\n' ou '\0')
    la t0, buffer           # t0 = ponteiro para o início da string
    li t1, 0                # t1 = contador de comprimento

find_end:
    lb t2, 0(t0)            # carrega o caractere atual
    beqz t2, found_end       # se for '\0', terminamos
    addi t0, t0, 1          # move para o próximo caractere
    addi t1, t1, 1          # incrementa contador
    j find_end

found_end:
    # Agora t0 aponta para o '\0' e t1 tem o comprimento
    # Precisamos começar a copiar do último caractere válido

    # Preparar para inverter
    la t2, buffer           # t2 = ponteiro para início da string original
    la t3, buffer_res       # t3 = ponteiro para início da string invertida
    add t4, t2, t1          # t4 = ponteiro para o final da string original (após o último caractere)
    addi t4, t4, -1         # t4 = ponteiro para o último caractere válido

invert_loop:
    blt t4, t2, end_invert  # se t4 < t2 (início), terminamos
    lb t5, 0(t4)            # carrega caractere da posição atual
    sb t5, 0(t3)            # armazena na string invertida
    addi t4, t4, -1         # move para o caractere anterior
    addi t3, t3, 1          # move para a próxima posição na string invertida
    j invert_loop

end_invert:
    # Adicionar terminador nulo à string invertida
    sb zero, 0(t3)

    # Imprimir mensagem de resultado
    li a7, 4
    la a0, str2
    ecall

    # Imprimir string invertida
    li a7, 4
    la a0, buffer_res
    ecall

    # Terminar programa
    li a7, 10
    ecall
