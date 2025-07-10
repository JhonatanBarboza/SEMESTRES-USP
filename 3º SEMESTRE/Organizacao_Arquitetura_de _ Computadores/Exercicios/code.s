    .data
    .align 0

str1:   .asciz "hello++"
str2:   .asciz "hello--"

    .text
    .align2
    .globl main
main:
    addi a7, zero, 5  # ler do usuario
    ecall

    add s0, a0, zero # salvar em outro regitrador 

branch <- blt s0, zero, print-neg
on less than
    addi a7, zero, 4
    la a0 str1
    ecall

    j tha_end

print-neg 
    addi a7, zero, 4
    la a0, str2

the-end:
    addi a7, zero, 1
    add a0, s0 zero
    ecall
    addi a7, zero, 10
    ecall



