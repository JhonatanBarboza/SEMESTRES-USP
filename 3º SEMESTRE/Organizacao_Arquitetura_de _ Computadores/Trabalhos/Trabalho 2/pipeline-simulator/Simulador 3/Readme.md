# Simulador completo de pipeline RISC-V 

O simulador é uma ferramenta educacional completa que demonstra visualmente como funciona um pipeline de processador, incluindo os desafios e soluções para hazards. É perfeito para entender conceitos de arquitetura de computadores!

## 🎯 **Características Principais:**

### **Interface Visual Interativa**
- Editor de código Assembly integrado
- Visualização em tempo real das 5 etapas do pipeline (IF, ID, EX, MEM, WB)
- Animações e destacação das etapas ativas
- Painel de registradores com atualizações visuais
- Contador de ciclos e estatísticas de performance

### **Instruções Suportadas**
- **Tipo R**: `add`, `sub`, `mul`, `div`, `and`, `or`, `xor`
- **Tipo I**: `addi`, `lw` (load word)
- **Tipo S**: `sw` (store word)  
- **Tipo B**: `beq`, `bne` (branches)
- **Tipo J**: `jal`, `jalr` (jumps)

### **Detecção e Resolução de Hazards**

#### **Data Hazards**
- **RAW (Read After Write)**: Detecta quando uma instrução precisa do resultado de outra
- **Load-Use Hazard**: Identifica quando uma instrução precisa esperar um load
- **Forwarding**: Simula o forwarding automático para resolver RAW hazards
- **Stalls**: Implementa stalls obrigatórios para load-use hazards

#### **Control Hazards**
- **Branch Prediction**: Simula flush do pipeline quando branches são tomados
- **Pipeline Flush**: Limpa instruções incorretamente buscadas

### **Funcionalidades Educacionais**

#### **Modos de Execução**
- **Step-by-Step**: Avança ciclo por ciclo para análise detalhada
- **Auto Execute**: Execução automática com pausa de 1 segundo
- **Reset**: Reinicia completamente a simulação

#### **Informações Detalhadas**
- **Hazard Alerts**: Mostra exatamente qual hazard foi detectado
- **Performance Stats**: CPI (Cycles Per Instruction), total de stalls, etc.
- **Memory View**: Visualização da memória de instruções
- **Register Updates**: Destaque visual dos registradores modificados

### **Exemplos Pré-programados**
- **Básico**: Operações aritméticas simples
- **Hazards**: Demonstra diferentes tipos de hazards
- **Branch**: Exemplo com instruções de desvio

### **Controles e Atalhos**
- `Ctrl + Enter`: Iniciar simulação
- `Espaço`: Próximo ciclo (durante execução)
- `Ctrl + R`: Reset da simulação

## 🚀 **Como Usar:**

1. **Digite ou selecione** um programa Assembly RISC-V
2. **Clique "Executar"** para compilar e carregar o programa
3. **Use "Próximo Ciclo"** para ver cada etapa do pipeline
4. **Observe** os hazards sendo detectados e resolvidos em tempo real
5. **Analise** as estatísticas de performance e comportamento do pipeline

