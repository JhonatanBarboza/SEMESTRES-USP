class PipelineSimulator {
    constructor() {
        this.reset();
        this.forwardingEnabled = true;
        this.hazardDetectionEnabled = true;
    }
    
    reset() {
        // Estado do processador
        this.registers = new Array(32).fill(0);
        this.memory = {};
        this.pc = 0;
        
        // Pipeline
        this.if = { instruction: null, active: false };
        this.id = { instruction: null, active: false };
        this.ex = { instruction: null, active: false };
        this.mem = { instruction: null, active: false };
        this.wb = { instruction: null, active: false };
        
        // Histórico para visualização
        this.pipelineHistory = [];
        this.cycleCount = 0;
        this.completedInstructions = 0;
        this.stallCount = 0;
        this.forwardingCount = 0;
        
        // Controle de hazards
        this.stallPipeline = false;
        this.currentHazards = [];
        
        // Programa
        this.program = [];
        this.originalCode = '';
    }
    
    loadCode(code) {
        this.reset();
        this.originalCode = code;
        
        // Simples parser para instruções RISC-V (simplificado para demonstração)
        const lines = code.split('\n');
        this.program = lines
            .map(line => line.trim())
            .filter(line => line.length > 0 && !line.startsWith('#'))
            .map(line => {
                // Extrai o nome da instrução e os operandos
                const [instr, ...operands] = line.split(/\s+|,\s*/);
                return { raw: line, instr, operands };
            });
    }
    
    isLoaded() {
        return this.program.length > 0;
    }
    
    isFinished() {
        return this.pc >= this.program.length && 
               !this.if.active && 
               !this.id.active && 
               !this.ex.active && 
               !this.mem.active && 
               !this.wb.active;
    }
    
    step() {
        if (this.isFinished()) return;
        
        this.currentHazards = [];
        this.cycleCount++;
        
        // WB stage
        if (this.wb.active) {
            this.writeBack();
            this.completedInstructions++;
        }
        
        // MEM stage
        if (this.mem.active) {
            this.memoryAccess();
        }
        
        // EX stage
        if (this.ex.active) {
            this.execute();
        }
        
        // ID stage
        if (this.id.active) {
            this.instructionDecode();
        }
        
        // IF stage
        if (this.if.active && !this.stallPipeline) {
            this.instructionFetch();
        }
        
        // Verifica hazards entre as instruções no pipeline
        this.detectHazards();
        
        // Atualiza o histórico do pipeline
        this.updatePipelineHistory();
        
        // Reseta o stall se não houver mais hazards
        if (this.currentHazards.length === 0) {
            this.stallPipeline = false;
        }
    }
    
    instructionFetch() {
        if (this.pc < this.program.length) {
            this.if = {
                instruction: this.program[this.pc],
                active: true,
                pc: this.pc
            };
            this.pc++;
        } else {
            this.if = { instruction: null, active: false };
        }
    }
    
    instructionDecode() {
        // Verifica se precisa fazer stall (hazard detectado)
        if (this.stallPipeline) {
            this.id.stalled = true;
            this.stallCount++;
            return;
        }
        
        this.id.stalled = false;
        this.ex = {
            instruction: this.id.instruction,
            active: true,
            pc: this.id.pc
        };
        
        this.id = { instruction: null, active: false };
    }
    
    execute() {
        const instr = this.ex.instruction;
        
        // Simulação simplificada da execução
        if (instr) {
            switch (instr.instr) {
                case 'add':
                case 'addi':
                case 'sub':
                case 'lw':
                case 'sw':
                case 'beq':
                    // Apenas marca como executada para esta demonstração
                    this.ex.executed = true;
                    break;
            }
        }
        
        this.mem = {
            instruction: this.ex.instruction,
            active: true,
            pc: this.ex.pc
        };
        
        this.ex = { instruction: null, active: false };
    }
    
    memoryAccess() {
        const instr = this.mem.instruction;
        
        // Simulação simplificada do acesso à memória
        if (instr) {
            if (instr.instr === 'lw') {
                // Carrega da memória
                const addr = 0; // Simplificado
                this.mem.loadedValue = this.memory[addr] || 0;
            } else if (instr.instr === 'sw') {
                // Armazena na memória
                const addr = 0; // Simplificado
                const value = 0; // Simplificado
                this.memory[addr] = value;
            }
        }
        
        this.wb = {
            instruction: this.mem.instruction,
            active: true,
            pc: this.mem.pc
        };
        
        this.mem = { instruction: null, active: false };
    }
    
    writeBack() {
        const instr = this.wb.instruction;
        
        // Simulação simplificada do write back
        if (instr) {
            if (instr.instr === 'add' || instr.instr === 'addi' || instr.instr === 'sub') {
                // Escreve no registrador de destino
                const rd = parseInt(instr.operands[0].substring(1));
                this.registers[rd] = 0; // Valor simplificado
            } else if (instr.instr === 'lw') {
                // Escreve o valor carregado no registrador
                const rd = parseInt(instr.operands[0].substring(1));
                this.registers[rd] = this.wb.loadedValue;
            }
        }
        
        this.wb = { instruction: null, active: false };
    }
    
    detectHazards() {
        this.currentHazards = [];
        
        // Data hazard entre ID e EX (RAW - Read After Write)
        if (this.id.active && this.ex.active) {
            const idInstr = this.id.instruction;
            const exInstr = this.ex.instruction;
            
            // Verifica se a instrução em EX escreve em um registrador que a instrução em ID lê
            if (this.writesToRegister(exInstr) && this.readsFromRegister(idInstr, exInstr.operands[0])) {
                const hazard = {
                    type: 'Data Hazard (RAW)',
                    instruction: idInstr.raw,
                    action: this.forwardingEnabled ? 'Forwarding aplicado' : 'Stall inserido'
                };
                
                this.currentHazards.push(hazard);
                
                if (this.forwardingEnabled) {
                    this.forwardingCount++;
                } else if (this.hazardDetectionEnabled) {
                    this.stallPipeline = true;
                    this.stallCount++;
                }
            }
        }
        
        // Control hazard (branch)
        if (this.ex.active && this.ex.instruction.instr === 'beq') {
            const hazard = {
                type: 'Control Hazard (Branch)',
                instruction: this.ex.instruction.raw,
                action: 'Stall inserido até resolução do branch'
            };
            
            this.currentHazards.push(hazard);
            
            if (this.hazardDetectionEnabled) {
                this.stallPipeline = true;
                this.stallCount++;
            }
        }
    }
    
    writesToRegister(instr) {
        if (!instr) return false;
        
        return ['add', 'addi', 'sub', 'lw'].includes(instr.instr);
    }
    
    readsFromRegister(instr, reg) {
        if (!instr) return false;
        
        // Verifica se a instrução lê do registrador especificado
        const operands = instr.operands;
        
        if (instr.instr === 'add' || instr.instr === 'sub') {
            return operands[1] === reg || operands[2] === reg;
        } else if (instr.instr === 'addi' || instr.instr === 'lw' || instr.instr === 'sw' || instr.instr === 'beq') {
            return operands[1] === reg;
        }
        
        return false;
    }
    
    updatePipelineHistory() {
        const currentState = [
            this.if.active ? { ...this.if, stage: 'IF' } : null,
            this.id.active ? { ...this.id, stage: 'ID' } : null,
            this.ex.active ? { ...this.ex, stage: 'EX' } : null,
            this.mem.active ? { ...this.mem, stage: 'MEM' } : null,
            this.wb.active ? { ...this.wb, stage: 'WB' } : null
        ];
        
        this.pipelineHistory.push(currentState);
    }
    
    // Métodos para a interface
    getPipelineHistory() {
        return this.pipelineHistory;
    }
    
    getRegisters() {
        return [...this.registers];
    }
    
    getMemory() {
        return { ...this.memory };
    }
    
    getCycleCount() {
        return this.cycleCount;
    }
    
    getCompletedInstructions() {
        return this.completedInstructions;
    }
    
    getStallCount() {
        return this.stallCount;
    }
    
    getForwardingCount() {
        return this.forwardingCount;
    }
    
    getCurrentHazards() {
        return [...this.currentHazards];
    }
}