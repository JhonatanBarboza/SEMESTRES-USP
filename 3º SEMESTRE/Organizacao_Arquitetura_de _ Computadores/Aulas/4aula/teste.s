.data
    # Dados do programa
    .align 2       # Alinhamento para palavras (4 bytes)
msg1:   .asciz "Digite o número cujo fatorial você quer calcular: "
msg2:   .asciz "Resultado: "
msg3:   .asciz "! = "
    
.text
    .align 2       # Alinhamento para instruções
    .globl main

# Função fatorial
# Entrada: a0 - número para calcular fatorial
# Saída: a0 - resultado do fatorial
fatorial:
    li t1, 1           # Resultado inicial (0! = 1)
    mv t0, a0          # Contador (n)
    
fatorial_loop:
    beq t0, zero, fatorial_end
    mul t1, t1, t0     # t1 = t1 * t0
    addi t0, t0, -1    # Decrementa contador
    j fatorial_loop

fatorial_end:
    mv a0, t1          # Retorna resultado em a0
    ret

main:
    # Configuração da pilha (alinhada em 16 bytes)
    addi sp, sp, -16
    sw ra, 12(sp)
    
    # Imprime mensagem 1
    la a0, msg1
    li a7, 4
    ecall
    
    # Lê número do usuário
    li a7, 5
    ecall
    
    # Chama função fatorial
    call fatorial
    mv t0, a0          # Salva resultado em t0
    
    # Imprime resultado
    la a0, msg2
    li a7, 4
    ecall
    
    # Recupera o valor original (estava em a0 antes do ecall)
    li a7, 5           # Re-lê o valor (alternativa: guardar em s0)
    ecall
    mv a1, a0          # Guarda em a1
    
    la a0, msg3
    li a7, 4
    ecall
    
    mv a0, t0          # Recupera resultado do fatorial
    li a7, 1
    ecall
    
    # Finaliza programa
    lw ra, 12(sp)
    addi sp, sp, 16
    li a7, 10
    ecall