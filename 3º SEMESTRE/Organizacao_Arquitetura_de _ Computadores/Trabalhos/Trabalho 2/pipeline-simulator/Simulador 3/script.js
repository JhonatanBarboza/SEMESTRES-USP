// Estado do simulador
let simulator = {
    instructions: [],
    registers: new Array(32).fill(0),
    memory: new Array(1024).fill(0),
    pc: 0,
    cycle: 0,
    pipeline: {
        IF: null,
        ID: null,
        EX: null,
        MEM: null,
        WB: null
    },
    running: false,
    autoRun: false,
    stats: {
        instructions: 0,
        cycles: 0,
        stalls: 0
    },
    labels: {},
    stallCounter: 0,
    hazardInfo: [],
    forwardingPaths: [],
};
simulator.memoryAccess = {
    lastRead: null,
    lastWrite: null,
    accessHistory: []
};


// Mapeamento de registradores RISC-V com nomes convencionais
const regMap = {
    // Registrador zero (sempre 0)
    'x0': 0, 'zero': 0,
    
    // Return address
    'x1': 1, 'ra': 1,
    
    // Stack pointer
    'x2': 2, 'sp': 2,
    
    // Global pointer
    'x3': 3, 'gp': 3,
    
    // Thread pointer
    'x4': 4, 'tp': 4,
    
    // Temporários t0-t2
    'x5': 5, 't0': 5,
    'x6': 6, 't1': 6,
    'x7': 7, 't2': 7,
    
    // Saved register / frame pointer
    'x8': 8, 's0': 8, 'fp': 8,
    
    // Saved register
    'x9': 9, 's1': 9,
    
    // Function arguments / return values a0-a1
    'x10': 10, 'a0': 10,
    'x11': 11, 'a1': 11,
    
    // Function arguments a2-a7
    'x12': 12, 'a2': 12,
    'x13': 13, 'a3': 13,
    'x14': 14, 'a4': 14,
    'x15': 15, 'a5': 15,
    'x16': 16, 'a6': 16,
    'x17': 17, 'a7': 17,
    
    // Saved registers s2-s11
    'x18': 18, 's2': 18,
    'x19': 19, 's3': 19,
    'x20': 20, 's4': 20,
    'x21': 21, 's5': 21,
    'x22': 22, 's6': 22,
    'x23': 23, 's7': 23,
    'x24': 24, 's8': 24,
    'x25': 25, 's9': 25,
    'x26': 26, 's10': 26,
    'x27': 27, 's11': 27,
    
    // Temporários t3-t6
    'x28': 28, 't3': 28,
    'x29': 29, 't4': 29,
    'x30': 30, 't5': 30,
    'x31': 31, 't6': 31
};

// Função para obter o nome convencional do registrador
function getRegisterName(regNum) {
    const regNames = {
        0: 'zero', 1: 'ra', 2: 'sp', 3: 'gp', 4: 'tp',
        5: 't0', 6: 't1', 7: 't2', 8: 's0/fp', 9: 's1',
        10: 'a0', 11: 'a1', 12: 'a2', 13: 'a3', 14: 'a4',
        15: 'a5', 16: 'a6', 17: 'a7', 18: 's2', 19: 's3',
        20: 's4', 21: 's5', 22: 's6', 23: 's7', 24: 's8',
        25: 's9', 26: 's10', 27: 's11', 28: 't3', 29: 't4',
        30: 't5', 31: 't6'
    };
    return regNames[regNum] || `x${regNum}`;
}

// Função parseAssembly modificada para aceitar nomes convencionais
function parseAssembly(code) {
    const lines = code.split('\n').map(line => line.trim()).filter(line => line && !line.startsWith('#'));
    const instructions = [];
    const labels = {};
    
    // Primeira passada: identificar labels
    let instIndex = 0;
    for (let line of lines) {
        if (line.includes(':')) {
            const label = line.split(':')[0].trim();
            labels[label] = instIndex;
        } else {
            instIndex++;
        }
    }
    
    // Segunda passada: processar instruções
    for (let line of lines) {
        if (line.includes(':')) {
            line = line.split(':')[1].trim();
            if (!line) continue;
        }
        
        const parts = line.replace(/,/g, ' ').split(/\s+/);
        const opcode = parts[0].toLowerCase();
        
        let instruction = {
            opcode: opcode,
            original: line,
            type: getInstructionType(opcode)
        };
        
        switch (instruction.type) {
            case 'R':
                if (opcode === 'mul' || opcode === 'div') {
                    instruction.rd = regMap[parts[1].toLowerCase()];
                    instruction.rs1 = regMap[parts[2].toLowerCase()];
                    instruction.rs2 = regMap[parts[3].toLowerCase()];
                } else {
                    instruction.rd = regMap[parts[1].toLowerCase()];
                    instruction.rs1 = regMap[parts[2].toLowerCase()];
                    instruction.rs2 = regMap[parts[3].toLowerCase()];
                }
                break;
            case 'I':
                if (opcode === 'lw') {
                    instruction.rd = regMap[parts[1].toLowerCase()];
                    const match = parts[2].match(/(-?\d+)\(([a-zA-Z0-9]+)\)/);
                    instruction.offset = parseInt(match[1]);
                    instruction.rs1 = regMap[match[2].toLowerCase()];
                } else if (opcode === 'jalr') {
                    instruction.rd = regMap[parts[1].toLowerCase()];
                    if (parts.length > 2) {
                        if (parts[2].includes('(')) {
                            // Formato: jalr rd, offset(rs1)
                            const match = parts[2].match(/(-?\d+)\(([a-zA-Z0-9]+)\)/);
                            instruction.offset = parseInt(match[1]);
                            instruction.rs1 = regMap[match[2].toLowerCase()];
                        } else {
                            // Formato: jalr rd, rs1
                            instruction.rs1 = regMap[parts[2].toLowerCase()];
                            instruction.offset = 0;
                        }
                    } else {
                        // Formato: jalr rd (assume rs1=ra, offset=0)
                        instruction.rs1 = 1; // ra por padrão
                        instruction.offset = 0;
                    }
                } else {
                    instruction.rd = regMap[parts[1].toLowerCase()];
                    instruction.rs1 = regMap[parts[2].toLowerCase()];
                    instruction.imm = parseInt(parts[3]);
                }
                break;
            case 'S':
                instruction.rs2 = regMap[parts[1].toLowerCase()];
                const match = parts[2].match(/(-?\d+)\(([a-zA-Z0-9]+)\)/);
                instruction.offset = parseInt(match[1]);
                instruction.rs1 = regMap[match[2].toLowerCase()];
                break;
            case 'B':
                instruction.rs1 = regMap[parts[1].toLowerCase()];
                instruction.rs2 = regMap[parts[2].toLowerCase()];
                instruction.label = parts[3];
                instruction.target = labels[parts[3]];
                break;
            case 'J':
                // JAL rd, label
                instruction.rd = regMap[parts[1].toLowerCase()];
                instruction.label = parts[2];
                instruction.target = labels[parts[2]];
                break;
            case 'SYSTEM':
                // ECALL não precisa de operandos adicionais
                break;
        }
        
        instructions.push(instruction);
    }
    
    return { instructions, labels };
}

// Função getInstructionType modificada para incluir as novas instruções
function getInstructionType(opcode) {
    const types = {
        'add': 'R', 'sub': 'R', 'and': 'R', 'or': 'R', 'xor': 'R',
        'mul': 'R', 'div': 'R',  // Novas instruções de multiplicação/divisão
        'addi': 'I', 'lw': 'I',
        'sw': 'S',
        'beq': 'B', 'bne': 'B',
        'jal': 'J', 'jalr': 'I',  // Instruções de salto
        'ecall': 'SYSTEM'         // Chamada de sistema
    };
    return types[opcode] || 'R';
}

// Função getForwardedValue modificada para usar nomes convencionais no log
function getForwardedValue(reg, defaultValue) {
    // Verifica se podemos fazer forwarding do estágio MEM
    if (simulator.pipeline.MEM && simulator.pipeline.MEM.rd === reg && 
        simulator.pipeline.MEM.rd !== 0 && simulator.pipeline.MEM.opcode !== 'lw') {
        simulator.forwardingPaths.push(`MEM->EX: ${getRegisterName(reg)} = ${simulator.pipeline.MEM.result}`);
        return simulator.pipeline.MEM.result;
    }
    
    // Verifica se podemos fazer forwarding do estágio WB
    if (simulator.pipeline.WB && simulator.pipeline.WB.rd === reg && 
        simulator.pipeline.WB.rd !== 0) {
        simulator.forwardingPaths.push(`WB->EX: ${getRegisterName(reg)} = ${simulator.pipeline.WB.result}`);
        return simulator.pipeline.WB.result;
    }
    
    return defaultValue;
}


function detectHazards() {
    const hazards = [];
    const stages = simulator.pipeline;
    
    // Data Hazard Detection
    if (stages.ID && stages.EX) {
        const idInst = stages.ID;
        const exInst = stages.EX;
        
        // RAW hazard
        if (exInst.rd !== undefined && exInst.rd !== 0) {
            if ((idInst.rs1 === exInst.rd) || (idInst.rs2 === exInst.rd)) {
                hazards.push({
                    type: 'RAW',
                    description: `Hazard RAW: ${idInst.original} precisa do resultado de ${exInst.original}`,
                    canForward: exInst.opcode !== 'lw'
                });
            }
        }
    }
    
    // Load-Use Hazard - verifica EX stage para lw
    if (stages.ID && stages.EX && stages.EX.opcode === 'lw') {
        const idInst = stages.ID;
        const exInst = stages.EX;
        
        if (exInst.rd !== undefined && exInst.rd !== 0 &&
            ((idInst.rs1 === exInst.rd) || (idInst.rs2 === exInst.rd))) {
            hazards.push({
                type: 'LOAD_USE',
                description: `Load-Use Hazard: ${idInst.original} precisa esperar ${exInst.original}`,
                canForward: false
            });
        }
    }
    
    return hazards;
}

// Função executeInstruction modificada para registrar acessos à memória
function executeInstruction(instruction, stage) {
    if (!instruction) return null;
    
    let result = { ...instruction };
    
    switch (stage) {
        case 'EX':
            simulator.forwardingPaths = []; // Reset forwarding paths
            
            let rs1Val = simulator.registers[instruction.rs1] || 0;
            let rs2Val = simulator.registers[instruction.rs2] || 0;
            
            // Aplicar forwarding se disponível
            if (instruction.rs1 !== undefined) {
                rs1Val = getForwardedValue(instruction.rs1, rs1Val);
            }
            if (instruction.rs2 !== undefined) {
                rs2Val = getForwardedValue(instruction.rs2, rs2Val);
            }
            
            switch (instruction.opcode) {
                case 'add':
                    result.result = rs1Val + rs2Val;
                    break;
                case 'sub':
                    result.result = rs1Val - rs2Val;
                    break;
                case 'and':
                    result.result = rs1Val & rs2Val;
                    break;
                case 'or':
                    result.result = rs1Val | rs2Val;
                    break;
                case 'xor':
                    result.result = rs1Val ^ rs2Val;
                    break;
                case 'mul':
                    result.result = rs1Val * rs2Val;
                    result.executionTime = 3;
                    break;
                case 'div':
                    if (rs2Val === 0) {
                        result.result = -1;
                        result.error = "Division by zero";
                    } else {
                        result.result = Math.floor(rs1Val / rs2Val);
                    }
                    result.executionTime = 5;
                    break;
                case 'addi':
                    result.result = rs1Val + instruction.imm;
                    break;
                case 'lw':
                    result.address = rs1Val + instruction.offset;
                    break;
                case 'sw':
                    result.address = rs1Val + instruction.offset;
                    result.data = rs2Val;
                    break;
                case 'beq':
                    result.taken = rs1Val === rs2Val;
                    break;
                case 'bne':
                    result.taken = rs1Val !== rs2Val;
                    break;
                case 'jal':
                    result.result = simulator.pc * 4;
                    result.taken = true;
                    break;
                case 'jalr':
                    result.result = simulator.pc * 4;
                    result.targetAddress = (rs1Val + instruction.offset) / 4;
                    result.taken = true;
                    break;
                case 'ecall':
                    result.systemCall = true;
                    const syscallCode = simulator.registers[10];
                    const arg = simulator.registers[11];
                    
                    switch (syscallCode) {
                        case 1:
                            console.log(`ECALL Print: ${arg}`);
                            result.output = `Print: ${arg}`;
                            break;
                        case 10:
                            result.exit = true;
                            result.output = "Program exit";
                            break;
                        default:
                            result.output = `Unknown syscall: ${syscallCode}`;
                    }
                    break;
            }
            break;
            
        case 'MEM':
            if (instruction.opcode === 'lw') {
                result.result = simulator.memory[instruction.address] || 0;
                // Registrar acesso de leitura
                recordMemoryAccess(instruction.address, 'read', result.result);
            } else if (instruction.opcode === 'sw') {
                simulator.memory[instruction.address] = instruction.data;
                // Registrar acesso de escrita
                recordMemoryAccess(instruction.address, 'write', instruction.data);
            }
            break;
            
        case 'WB':
            if (instruction.rd !== undefined && instruction.rd !== 0) {
                simulator.registers[instruction.rd] = instruction.result;
            }
            break;
    }
    
    return result;
}

// Função para atualizar a visualização da memória de dados
function updateDataMemory() {
    const memDiv = document.getElementById('dataMemory');
    if (!memDiv) return;
    
    memDiv.innerHTML = '';
    
    // Mostrar apenas posições de memória que têm valores não-zero ou foram acessadas
    const relevantAddresses = new Set();
    
    // Adicionar endereços com valores não-zero
    for (let i = 0; i < simulator.memory.length; i++) {
        if (simulator.memory[i] !== 0) {
            relevantAddresses.add(i);
        }
    }
    
    // Adicionar endereços do histórico de acesso
    simulator.memoryAccess.accessHistory.forEach(access => {
        relevantAddresses.add(access.address);
    });
    
    // Se não há endereços relevantes, mostrar os primeiros 8
    if (relevantAddresses.size === 0) {
        for (let i = 0; i < 8; i++) {
            relevantAddresses.add(i);
        }
    }
    
    // Converter para array e ordenar
    const sortedAddresses = Array.from(relevantAddresses).sort((a, b) => a - b);
    
    sortedAddresses.forEach(address => {
        const div = document.createElement('div');
        div.className = 'memory-line';
        
        // Aplicar classes de destaque
        if (simulator.memoryAccess.lastRead === address) {
            div.classList.add('last-read');
        }
        if (simulator.memoryAccess.lastWrite === address) {
            div.classList.add('last-write');
        }
        
        // Verificar se foi acessado recentemente (últimos 3 ciclos)
        const recentAccess = simulator.memoryAccess.accessHistory.find(
            access => access.address === address && 
            (simulator.cycle - access.cycle) <= 3
        );
        
        if (recentAccess) {
            div.classList.add(`recent-${recentAccess.type}`);
        }
        
        const value = simulator.memory[address] || 0;
        div.innerHTML = `
            <span class="memory-address">0x${address.toString(16).padStart(3, '0')}</span>
            <span class="memory-value">${value}</span>
            <span class="memory-hex">0x${value.toString(16).padStart(8, '0')}</span>
        `;
        
        memDiv.appendChild(div);
    });
    
    // Adicionar informações de acesso recente
    if (simulator.memoryAccess.accessHistory.length > 0) {
        const infoDiv = document.createElement('div');
        infoDiv.className = 'memory-info';
        
        const lastAccess = simulator.memoryAccess.accessHistory[simulator.memoryAccess.accessHistory.length - 1];
        infoDiv.innerHTML = `
            <strong>Último acesso:</strong> ${lastAccess.type.toUpperCase()} 
            em 0x${lastAccess.address.toString(16).padStart(3, '0')} 
            (Ciclo ${lastAccess.cycle})
        `;
        
        memDiv.appendChild(infoDiv);
    }
}

// Função para registrar acesso à memória
function recordMemoryAccess(address, type, value = null) {
    // Atualizar último acesso
    if (type === 'read') {
        simulator.memoryAccess.lastRead = address;
    } else if (type === 'write') {
        simulator.memoryAccess.lastWrite = address;
    }
    
    // Adicionar ao histórico
    const accessRecord = {
        address: address,
        type: type,
        cycle: simulator.cycle,
        value: value
    };
    
    simulator.memoryAccess.accessHistory.push(accessRecord);
    
    // Manter apenas os últimos 10 acessos
    if (simulator.memoryAccess.accessHistory.length > 10) {
        simulator.memoryAccess.accessHistory.shift();
    }
}


// Função helper para criar div de output do sistema
function createSystemOutputDiv() {
    const container = document.querySelector('.simulator-container');
    const outputDiv = document.createElement('div');
    outputDiv.id = 'systemOutput';
    outputDiv.style.cssText = `
        background: #f8f9fa;
        border: 2px solid #e9ecef;
        border-radius: 8px;
        padding: 15px;
        margin: 10px 0;
        font-family: 'Courier New', monospace;
        max-height: 200px;
        overflow-y: auto;
    `;
    outputDiv.innerHTML = '<h3>System Call Output:</h3>';
    container.appendChild(outputDiv);
    return outputDiv;
}

// Função stepPipeline modificada para tratar saltos e system calls
function stepPipeline() {
    const hazards = detectHazards();
    simulator.hazardInfo = hazards;
    
    // Verificar se precisa de stall (Load-Use hazard)
    const needStall = hazards.some(h => h.type === 'LOAD_USE');
    
    if (needStall) {
        simulator.stats.stalls++;
        // Inserir bubble (NOP) no pipeline
        simulator.pipeline.WB = executeInstruction(simulator.pipeline.MEM, 'WB');
        simulator.pipeline.MEM = executeInstruction(simulator.pipeline.EX, 'MEM');
        simulator.pipeline.EX = null; // bubble
        // ID e IF ficam congelados (não avançam)
        
    } else {
        // Pipeline normal - avança todos os estágios
        simulator.pipeline.WB = executeInstruction(simulator.pipeline.MEM, 'WB');
        simulator.pipeline.MEM = executeInstruction(simulator.pipeline.EX, 'MEM');
        simulator.pipeline.EX = executeInstruction(simulator.pipeline.ID, 'EX');
        simulator.pipeline.ID = simulator.pipeline.IF;
        
        // Branch handling
        if (simulator.pipeline.EX && 
            (simulator.pipeline.EX.opcode === 'beq' || simulator.pipeline.EX.opcode === 'bne') &&
            simulator.pipeline.EX.taken) {
            simulator.pc = simulator.pipeline.EX.target;
            // Flush pipeline
            simulator.pipeline.IF = null;
            simulator.pipeline.ID = null;
        }
        
        // JAL handling
        if (simulator.pipeline.EX && simulator.pipeline.EX.opcode === 'jal' && simulator.pipeline.EX.taken) {
            simulator.pc = simulator.pipeline.EX.target;
            // Flush pipeline
            simulator.pipeline.IF = null;
            simulator.pipeline.ID = null;
        }
        
        // JALR handling
        if (simulator.pipeline.EX && simulator.pipeline.EX.opcode === 'jalr' && simulator.pipeline.EX.taken) {
            simulator.pc = simulator.pipeline.EX.targetAddress;
            // Flush pipeline
            simulator.pipeline.IF = null;
            simulator.pipeline.ID = null;
        }
        
        // ECALL handling
        if (simulator.pipeline.EX && simulator.pipeline.EX.opcode === 'ecall') {
            if (simulator.pipeline.EX.exit) {
                simulator.running = false;
                document.getElementById('stepBtn').disabled = true;
                simulator.autoRun = false;
                document.getElementById('autoBtn').textContent = '⚡ Auto Executar';
                alert('Programa terminado por ECALL exit');
                return;
            }
            if (simulator.pipeline.EX.output) {
                // Mostrar output da system call
                const outputDiv = document.getElementById('systemOutput') || createSystemOutputDiv();
                outputDiv.innerHTML += `<div>Ciclo ${simulator.cycle}: ${simulator.pipeline.EX.output}</div>`;
            }
        }
        
        // Fetch next instruction
        if (simulator.pc < simulator.instructions.length) {
            simulator.pipeline.IF = { ...simulator.instructions[simulator.pc] };
            simulator.pc++;
        } else {
            simulator.pipeline.IF = null;
        }
    }
    
    simulator.cycle++;
    simulator.stats.cycles++;
    
    // Contar instruções completadas
    if (simulator.pipeline.WB) {
        simulator.stats.instructions++;
    }
}


// Função updateDisplay modificada para incluir a atualização da memória
function updateDisplay() {
    // Update cycle counter
    document.getElementById('cycleCounter').textContent = `Ciclo: ${simulator.cycle}`;
    
    // Update pipeline stages
    const stages = ['IF', 'ID', 'EX', 'MEM', 'WB'];
    stages.forEach(stage => {
        const element = document.getElementById(`stage-${stage.toLowerCase()}`);
        const content = document.getElementById(`${stage.toLowerCase()}-content`);
        
        element.classList.remove('active', 'stall');
        
        if (simulator.pipeline[stage]) {
            element.classList.add('active');
            content.textContent = simulator.pipeline[stage].original || stage;
        } else {
            content.textContent = getStageDescription(stage);
        }
        
        // Highlight stalls para Load-Use hazards
        const loadUseHazard = simulator.hazardInfo.some(h => h.type === 'LOAD_USE');
        if (loadUseHazard && (stage === 'IF' || stage === 'ID')) {
            element.classList.add('stall');
        }
    });
    
    // Update hazard information
    const hazardDiv = document.getElementById('hazardInfo');
    hazardDiv.innerHTML = '';
    
    simulator.hazardInfo.forEach(hazard => {
        const div = document.createElement('div');
        div.className = hazard.canForward ? 'forwarding-active' : 'hazard-detected';
        div.textContent = hazard.description;
        if (hazard.canForward) {
            div.textContent += ' (Resolvido com Forwarding)';
        } else {
            div.textContent += ' (Requer Stall)';
        }
        hazardDiv.appendChild(div);
    });
    
    // Show forwarding paths
    if (simulator.forwardingPaths.length > 0) {
        const forwardingDiv = document.createElement('div');
        forwardingDiv.className = 'forwarding-active';
        forwardingDiv.innerHTML = '<strong>Forwarding:</strong><br>' + simulator.forwardingPaths.join('<br>');
        hazardDiv.appendChild(forwardingDiv);
    }
    
    // Update registers
    updateRegisters();
    
    // Update instruction memory
    updateInstructionMemory();
    
    // Update data memory - NOVA FUNCIONALIDADE
    updateDataMemory();
    
    // Update performance stats
    updatePerformanceStats();
}

// Função updateRegisters modificada para mostrar apenas nomes convencionais
function updateRegisters() {
    const grid = document.getElementById('registersGrid');
    grid.innerHTML = '';
    
    for (let i = 0; i < 32; i++) {
        const regDiv = document.createElement('div');
        regDiv.className = 'register';
        
        // Destacar registradores importantes
        if (i === 0) regDiv.classList.add('zero-register');
        else if (i >= 5 && i <= 7 || i >= 28 && i <= 31) regDiv.classList.add('temp-register');
        else if (i >= 8 && i <= 9 || i >= 18 && i <= 27) regDiv.classList.add('saved-register');
        else if (i >= 10 && i <= 17) regDiv.classList.add('arg-register');
        else regDiv.classList.add('special-register');
        
        regDiv.innerHTML = `<strong>${getRegisterName(i)}</strong><br>${simulator.registers[i]}`;
        grid.appendChild(regDiv);
    }
}

function updateInstructionMemory() {
    const memDiv = document.getElementById('instructionMemory');
    memDiv.innerHTML = '';
    
    simulator.instructions.forEach((inst, index) => {
        const div = document.createElement('div');
        div.className = 'instruction-line';
        if (index === simulator.pc - 1) {
            div.classList.add('current');
        }
        div.textContent = `${index}: ${inst.original}`;
        memDiv.appendChild(div);
    });
}

function updatePerformanceStats() {
    document.getElementById('instCount').textContent = simulator.stats.instructions;
    document.getElementById('cycleCount').textContent = simulator.stats.cycles;
    document.getElementById('stallCount').textContent = simulator.stats.stalls;
    
    const cpi = simulator.stats.instructions > 0 ? 
        (simulator.stats.cycles / simulator.stats.instructions).toFixed(2) : '0.00';
    document.getElementById('cpiValue').textContent = cpi;
}

function getStageDescription(stage) {
    const descriptions = {
        'IF': 'Instruction Fetch',
        'ID': 'Instruction Decode',
        'EX': 'Execute',
        'MEM': 'Memory Access',
        'WB': 'Write Back'
    };
    return descriptions[stage] || stage;
}

// Função startSimulation modificada para resetar o histórico de memória
function startSimulation() {
    const code = document.getElementById('codeEditor').value;
    
    try {
        const parsed = parseAssembly(code);
        simulator.instructions = parsed.instructions;
        simulator.labels = parsed.labels;
        simulator.running = true;
        simulator.pc = 0;
        simulator.cycle = 0;
        simulator.stats = { instructions: 0, cycles: 0, stalls: 0 };
        simulator.stallCounter = 0;
        simulator.hazardInfo = [];
        simulator.forwardingPaths = [];
        
        // Reset memory access tracking - NOVO
        simulator.memoryAccess = {
            lastRead: null,
            lastWrite: null,
            accessHistory: []
        };
        
        // Reset pipeline
        simulator.pipeline = {
            IF: null,
            ID: null,
            EX: null,
            MEM: null,
            WB: null
        };
        
        // Reset registers (except x0)
        simulator.registers = new Array(32).fill(0);
        simulator.memory = new Array(1024).fill(0);
        
        document.getElementById('stepBtn').disabled = false;
        updateDisplay();
        
        alert(`Simulação iniciada! ${simulator.instructions.length} instruções carregadas.`);
        
    } catch (error) {
        alert(`Erro ao analisar o código: ${error.message}`);
    }
}

function stepForward() {
    if (!simulator.running) return;
    
    // Verificar se a simulação terminou
    const pipelineEmpty = !simulator.pipeline.IF && !simulator.pipeline.ID && 
                            !simulator.pipeline.EX && !simulator.pipeline.MEM && 
                            !simulator.pipeline.WB;
    
    if (simulator.pc >= simulator.instructions.length && pipelineEmpty) {
        simulator.running = false;
        document.getElementById('stepBtn').disabled = true;
        simulator.autoRun = false;
        document.getElementById('autoBtn').textContent = '⚡ Auto Executar';
        alert('Simulação concluída!');
        return;
    }
    
    stepPipeline();
    updateDisplay();
    
    // Continue auto execution if enabled
    if (simulator.autoRun) {
        setTimeout(stepForward, 1000);
    }
}

// Função resetSimulation modificada para limpar o histórico de memória
function resetSimulation() {
    simulator.running = false;
    simulator.autoRun = false;
    simulator.pc = 0;
    simulator.cycle = 0;
    simulator.stats = { instructions: 0, cycles: 0, stalls: 0 };
    simulator.stallCounter = 0;
    simulator.hazardInfo = [];
    simulator.forwardingPaths = [];
    
    // Reset memory access tracking - NOVO
    simulator.memoryAccess = {
        lastRead: null,
        lastWrite: null,
        accessHistory: []
    };
    
    // Reset pipeline
    simulator.pipeline = {
        IF: null,
        ID: null,
        EX: null,
        MEM: null,
        WB: null
    };
    
    // Reset registers and memory
    simulator.registers = new Array(32).fill(0);
    simulator.memory = new Array(1024).fill(0);
    
    document.getElementById('stepBtn').disabled = true;
    document.getElementById('autoBtn').textContent = '⚡ Auto Executar';
    
    updateDisplay();
}

function toggleAutoRun() {
    if (!simulator.running) {
        alert('Inicie a simulação primeiro!');
        return;
    }
    
    simulator.autoRun = !simulator.autoRun;
    const btn = document.getElementById('autoBtn');
    
    if (simulator.autoRun) {
        btn.textContent = '⏸️ Pausar';
        stepForward();
    } else {
        btn.textContent = '⚡ Auto Executar';
    }
}

// Initialize display
updateDisplay();

// Keyboard shortcuts
document.addEventListener('keydown', function(e) {
    if (e.key === 'Enter' && e.ctrlKey) {
        e.preventDefault();
        startSimulation();
    } else if (e.key === ' ' && simulator.running) {
        e.preventDefault();
        stepForward();
    } else if (e.key === 'r' && e.ctrlKey) {
        e.preventDefault();
        resetSimulation();
    }
});

// Exemplos atualizados usando nomes convencionais
const examples = {
    basic: `# Exemplo básico usando nomes convencionais
addi t0, zero, 10
addi t1, zero, 20
add t2, t0, t1
sub s0, t2, t0
sw s0, 0(sp)
lw a0, 0(sp)`,
    
    hazards: `# Exemplo com hazards
addi t0, zero, 100
addi t1, t0, 50
lw t2, 0(sp)
add s0, t2, t0 
sw s0, 4(sp)`,
    
    branch: `# Exemplo com branch
addi t0, zero, 10 
addi t1, zero, 10 
beq t0, t1, equal
addi s0, zero, 1 
equal:
addi s1, zero, 2`,

    forwarding: `# Exemplo de forwarding
addi t0, zero, 10
addi t1, t0, 5 
add t2, t1, t0
sub s0, t2, t1`,

};

// Add example selector
function addExampleSelector() {
    const controls = document.querySelector('.controls');
    const select = document.createElement('select');
    select.style.padding = '12px';
    select.style.borderRadius = '8px';
    select.style.border = '2px solid #e2e8f0';
    select.style.background = 'white';
    select.style.marginLeft = '10px';
    
    const defaultOption = document.createElement('option');
    defaultOption.value = '';
    defaultOption.textContent = '📝 Exemplos';
    select.appendChild(defaultOption);
    
    Object.keys(examples).forEach(key => {
        const option = document.createElement('option');
        option.value = key;
        option.textContent = key.charAt(0).toUpperCase() + key.slice(1);
        select.appendChild(option);
    });
    
    select.addEventListener('change', function() {
        if (this.value) {
            document.getElementById('codeEditor').value = examples[this.value];
            this.value = '';
        }
    });
    
    controls.appendChild(select);
}

// Initialize example selector
addExampleSelector();

