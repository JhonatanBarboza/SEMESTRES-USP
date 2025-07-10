.data  # Seção dedicada para declarar dados na memória
hello_msg:  .asciz  "Hello World!"  # Declara uma string terminada com \0 (null)

# hello_msg é um rótulo (label) que aponta para uma região de memória.
# .asciz armazena a string "Hello World!" na memória e adiciona automaticamente
# o caractere nulo (\0) ao final, indicando o término da string.

.text  # Seção que contém o código executável do programa
.globl main  # Define o ponto de entrada do programa como "main"

main:  # Ponto de entrada do programa
    li a7, 4  
    # "li" (load immediate) carrega um valor imediato no registrador.
    # "a7" é o registrador que armazena o código da syscall.
    # O valor 4 é o código da syscall para escrever uma string na saída padrão.

    la a0, hello_msg  
    # "la" (load address) carrega o endereço de memória do rótulo "hello_msg".
    # "a0" é o registrador que armazena o endereço da string a ser impressa.
    # "hello_msg" é o rótulo que aponta para a string "Hello World!".

    ecall  
    # Executa a syscall. O sistema operacional verifica o valor em "a7" (4) e
    # realiza a operação de escrever a string apontada por "a0" na saída padrão.

    li a7, 10  
    # "li" carrega o valor imediato 10 no registrador "a7".
    # O valor 10 é o código da syscall para encerrar o programa.

    ecall  
    # Executa a syscall. O sistema operacional verifica o valor em "a7" (10) e
    # encerra a execução do programa.