.global _start

_start:
    # Código para escrever na saída padrão (syscall write)
    li a7, 64          # Código da syscall para write (64)
    li a0, 1           # File descriptor 1 (stdout)
    la a1, msg         # Endereço da mensagem
    li a2, 13          # Tamanho da mensagem (13 bytes)
    ecall              # Chama a syscall

    # Código para finalizar o programa (syscall exit)
    li a7, 93          # Código da syscall para exit (93)
    li a0, 0           # Código de saída 0 (sucesso)
    ecall              # Chama a syscall

# Dados (mensagem a ser impressa)
.data
msg: .asciz "Olá, mundo!\n"

