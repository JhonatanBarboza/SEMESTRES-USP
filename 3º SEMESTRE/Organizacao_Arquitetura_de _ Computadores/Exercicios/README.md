# Tabelas de Referência para Assembly RISC-V

Aqui estão tabelas organizadas para facilitar a visualização dos principais comandos e estruturas em RISC-V:

## Tabela 1: Instruções Load e Store

| Instrução | Sintaxe           | Descrição                                 | Exemplo                 |
|-----------|-------------------|-------------------------------------------|-------------------------|
| **lw**    | `lw rd, offset(rs1)` | Carrega palavra (32 bits) com sinal       | `lw t0, 4(sp)`         |
| **lh**    | `lh rd, offset(rs1)` | Carrega meia palavra (16 bits) com sinal  | `lh t1, 8(s0)`         |
| **lb**    | `lb rd, offset(rs1)` | Carrega byte (8 bits) com sinal           | `lb t2, -3(gp)`        |
| **lhu**   | `lhu rd, offset(rs1)`| Carrega meia palavra sem sinal            | `lhu t3, 12(a0)`       |
| **lbu**   | `lbu rd, offset(rs1)`| Carrega byte sem sinal                    | `lbu t4, 1(ra)`        |
| **sw**    | `sw rs2, offset(rs1)`| Armazena palavra (32 bits)                | `sw a0, 0(sp)`         |
| **sh**    | `sh rs2, offset(rs1)`| Armazena meia palavra (16 bits)           | `sh a1, 6(t0)`         |
| **sb**    | `sb rs2, offset(rs1)`| Armazena byte (8 bits)                    | `sb a2, -4(s1)`        |

## Tabela 2: Syscalls Comuns (ambiente RARS/SPIM)

| Serviço | Código (a7) | Argumentos | Retorno | Descrição |
|---------|------------|------------|---------|-----------|
| print_int | 1 | a0 = inteiro | - | Imprime inteiro |
| print_float | 2 | fa0 = float | - | Imprime float |
| print_double | 3 | fa0 = double | - | Imprime double |
| print_string | 4 | a0 = endereço | - | Imprime string |
| read_int | 5 | - | a0 = inteiro | Lê inteiro |
| read_float | 6 | - | fa0 = float | Lê float |
| read_double | 7 | - | fa0 = double | Lê double |
| read_string | 8 | a0 = buffer, a1 = tamanho | - | Lê string |
| sbrk | 9 | a0 = bytes | a0 = endereço | Aloca memória |
| exit | 10 | - | - | Termina programa |
| print_char | 11 | a0 = caractere | - | Imprime caractere |
| read_char | 12 | - | a0 = caractere | Lê caractere |
| open_file | 13 | a0 = nome, a1 = flags | a0 = descritor | Abre arquivo |
| read_file | 14 | a0 = descritor, a1 = buffer, a2 = tamanho | a0 = bytes lidos | Lê arquivo |
| write_file | 15 | a0 = descritor, a1 = buffer, a2 = tamanho | a0 = bytes escritos | Escreve arquivo |
| close_file | 16 | a0 = descritor | - | Fecha arquivo |


## Tabela 3: Instruções de Branch (Desvio)

| Instrução | Sintaxe               | Descrição                          | Flags |
|-----------|-----------------------|------------------------------------|-------|
| **beq**   | `beq rs1, rs2, label` | Salta se igual (==)                | Z = 1 |
| **bne**   | `bne rs1, rs2, label` | Salta se diferente (!=)            | Z = 0 |
| **blt**   | `blt rs1, rs2, label` | Salta se menor (<, signed)         | N ≠ V |
| **bge**   | `bge rs1, rs2, label` | Salta se maior ou igual (≥, signed)| N = V |
| **bltu**  | `bltu rs1, rs2, label`| Salta se menor (unsigned)          | C = 1 |
| **bgeu**  | `bgeu rs1, rs2, label`| Salta se maior ou igual (unsigned) | C = 0 |


# Exemplos Práticos em RISC-V

## 1. Estrutura Condicional If-Else

```asm
# Implementação de if-else em RISC-V
# Equivalente a: if (a0 == a1) { ... } else { ... }

    beq a0, a1, if_true    # Se a0 == a1, vai para if_true
    
    # Bloco else (executa se condição falsa)
    addi t0, zero, 2       # Código do else
    j end_if               # Pula o bloco if
    
if_true:
    # Bloco if (executa se condição verdadeira)
    addi t0, zero, 1       # Código do if
    
end_if:
    # Continuação do programa
```

## 2. Loop While

```asm
# Implementação de while em RISC-V
# Equivalente a: while (i < 10) { ... }

    li t0, 0               # Inicializa i = 0
    li t1, 10              # Define o limite como 10
    
while_loop:
    bge t0, t1, end_while  # Se i >= 10, sai do loop
    
    # Corpo do while
    addi t0, t0, 1         # i++ (incrementa contador)
    
    j while_loop           # Volta para o início do loop
    
end_while:
    # Continuação do programa
```

## 3. Loop For

```asm
# Implementação de for em RISC-V
# Equivalente a: for (i=0; i<10; i++) { ... }

    li t0, 0               # Inicialização: i = 0
    li t1, 10              # Define o limite como 10
    
for_loop:
    bge t0, t1, end_for    # Condição: se i >= 10, sai
    
    # Corpo do for
    # (Coloque aqui as instruções do loop)
    
    addi t0, t0, 1         # Incremento: i++
    j for_loop             # Repete o loop
    
end_for:
    # Continuação do programa
```

## 4. Loop Do-While

```asm
# Implementação de do-while em RISC-V
# Equivalente a: do { ... } while (i < 10)

    li t0, 0               # Inicializa i = 0
    li t1, 10              # Define o limite como 10
    
do_while:
    # Corpo do loop
    # (Coloque aqui as instruções do loop)
    
    addi t0, t0, 1         # Incrementa i
    
    blt t0, t1, do_while   # Condição: repete se i < 10
    
    # Continuação do programa
```

## 5. Exemplo Completo: Soma de 1 a N

```asm
# Calcula a soma de 1 até N (valor em a0)
# Retorna resultado em a0

soma_1_a_n:
    li t0, 0               # t0 = soma = 0
    li t1, 1               # t1 = contador = 1
    
loop_soma:
    bgt t1, a0, fim_soma   # Se contador > N, termina
    
    add t0, t0, t1         # soma += contador
    addi t1, t1, 1         # contador++
    
    j loop_soma            # Repete
    
fim_soma:
    mv a0, t0              # Move resultado para a0
    ret                    # Retorna
```

## 6. Acesso à Memória (Load/Store)

```asm
# Exemplo de load/store com array
# Assume: a0 = endereço base do array
#         a1 = tamanho do array

    li t0, 0               # t0 = índice = 0
    li t1, 0               # t1 = soma = 0
    
loop_array:
    bge t0, a1, fim_array  # Se índice >= tamanho, termina
    
    slli t2, t0, 2         # Calcula offset (índice * 4)
    add t2, a0, t2         # t2 = endereço do elemento
    
    lw t3, 0(t2)           # Carrega elemento do array
    add t1, t1, t3         # soma += elemento
    
    addi t0, t0, 1         # índice++
    j loop_array
    
fim_array:
    # t1 contém a soma dos elementos
```

Cada exemplo mostra a implementação direta das estruturas de controle, com comentários explicativos. Você pode adaptar esses padrões básicos para necessidades específicas em seus programas RISC-V.

___

**Compilar**
riscv64-unknown-elf-as -march=rv32i -mabi=ilp32 -o NOME.o NOME.s
riscv64-unknown-elf-ld -m elf32lriscv -o NOME NOME.o

**Executar**
qemu-riscv32 NOME

**Excluir**
rm *.o NOME

**Excutar o simulador**
java -jar rars1_5.jar# Organizacao_Arquitetura_Computadores