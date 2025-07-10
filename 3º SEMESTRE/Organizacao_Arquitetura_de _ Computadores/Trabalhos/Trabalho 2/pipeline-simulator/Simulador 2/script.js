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
    forwardingPaths: []
};

// Mapeamento de registradores
const regMap = {
    'x0': 0, 'x1': 1, 'x2': 2, 'x3': 3, 'x4': 4, 'x5': 5, 'x6': 6, 'x7': 7,
    'x8': 8, 'x9': 9, 'x10': 10, 'x11': 11, 'x12': 12, 'x13': 13, 'x14': 14, 'x15': 15,
    'x16': 16, 'x17': 17, 'x18': 18, 'x19': 19, 'x20': 20, 'x21': 21, 'x22': 22, 'x23': 23,
    'x24': 24, 'x25': 25, 'x26': 26, 'x27': 27, 'x28': 28, 'x29': 29, 'x30': 30, 'x31': 31
};

// Função parseAssembly modificada para tratar as novas instruções
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
                    instruction.rd = regMap[parts[1]];
                    instruction.rs1 = regMap[parts[2]];
                    instruction.rs2 = regMap[parts[3]];
                } else {
                    instruction.rd = regMap[parts[1]];
                    instruction.rs1 = regMap[parts[2]];
                    instruction.rs2 = regMap[parts[3]];
                }
                break;
            case 'I':
                if (opcode === 'lw') {
                    instruction.rd = regMap[parts[1]];
                    const match = parts[2].match(/(-?\d+)\((x\d+)\)/);
                    instruction.offset = parseInt(match[1]);
                    instruction.rs1 = regMap[match[2]];
                } else if (opcode === 'jalr') {
                    instruction.rd = regMap[parts[1]];
                    if (parts.length > 2) {
                        if (parts[2].includes('(')) {
                            // Formato: jalr rd, offset(rs1)
                            const match = parts[2].match(/(-?\d+)\((x\d+)\)/);
                            instruction.offset = parseInt(match[1]);
                            instruction.rs1 = regMap[match[2]];
                        } else {
                            // Formato: jalr rd, rs1
                            instruction.rs1 = regMap[parts[2]];
                            instruction.offset = 0;
                        }
                    } else {
                        // Formato: jalr rd (assume rs1=x1, offset=0)
                        instruction.rs1 = 1; // x1 por padrão
                        instruction.offset = 0;
                    }
                } else {
                    instruction.rd = regMap[parts[1]];
                    instruction.rs1 = regMap[parts[2]];
                    instruction.imm = parseInt(parts[3]);
                }
                break;
            case 'S':
                instruction.rs2 = regMap[parts[1]];
                const match = parts[2].match(/(-?\d+)\((x\d+)\)/);
                instruction.offset = parseInt(match[1]);
                instruction.rs1 = regMap[match[2]];
                break;
            case 'B':
                instruction.rs1 = regMap[parts[1]];
                instruction.rs2 = regMap[parts[2]];
                instruction.label = parts[3];
                instruction.target = labels[parts[3]];
                break;
            case 'J':
                // JAL rd, label
                instruction.rd = regMap[parts[1]];
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

function getForwardedValue(reg, defaultValue) {
    // Verifica se podemos fazer forwarding do estágio MEM
    if (simulator.pipeline.MEM && simulator.pipeline.MEM.rd === reg && 
        simulator.pipeline.MEM.rd !== 0 && simulator.pipeline.MEM.opcode !== 'lw') {
        simulator.forwardingPaths.push(`MEM->EX: x${reg} = ${simulator.pipeline.MEM.result}`);
        return simulator.pipeline.MEM.result;
    }
    
    // Verifica se podemos fazer forwarding do estágio WB
    if (simulator.pipeline.WB && simulator.pipeline.WB.rd === reg && 
        simulator.pipeline.WB.rd !== 0) {
        simulator.forwardingPaths.push(`WB->EX: x${reg} = ${simulator.pipeline.WB.result}`);
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

// Função executeInstruction modificada para incluir as novas operações
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
                    result.executionTime = 3; // Multiplicação demora mais ciclos
                    break;
                case 'div':
                    if (rs2Val === 0) {
                        result.result = -1; // Divisão por zero
                        result.error = "Division by zero";
                    } else {
                        result.result = Math.floor(rs1Val / rs2Val);
                    }
                    result.executionTime = 5; // Divisão demora ainda mais ciclos
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
                    result.result = simulator.pc * 4; // Salva PC+4 no registrador
                    result.taken = true;
                    break;
                case 'jalr':
                    result.result = simulator.pc * 4; // Salva PC+4 no registrador
                    result.targetAddress = (rs1Val + instruction.offset) / 4; // Calcula endereço de destino
                    result.taken = true;
                    break;
                case 'ecall':
                    result.systemCall = true;
                    // Implementação básica de system call
                    // a0 (x10) = código da syscall, a1 (x11) = argumento
                    const syscallCode = simulator.registers[10]; // a0
                    const arg = simulator.registers[11]; // a1
                    
                    switch (syscallCode) {
                        case 1: // print integer
                            console.log(`ECALL Print: ${arg}`);
                            result.output = `Print: ${arg}`;
                            break;
                        case 10: // exit
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
            } else if (instruction.opcode === 'sw') {
                simulator.memory[instruction.address] = instruction.data;
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
    
    // Update performance stats
    updatePerformanceStats();
}

function updateRegisters() {
    const grid = document.getElementById('registersGrid');
    grid.innerHTML = '';
    
    for (let i = 0; i < 32; i++) {
        const regDiv = document.createElement('div');
        regDiv.className = 'register';
        regDiv.innerHTML = `<strong>x${i}</strong><br>${simulator.registers[i]}`;
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

function resetSimulation() {
    simulator.running = false;
    simulator.autoRun = false;
    simulator.pc = 0;
    simulator.cycle = 0;
    simulator.stats = { instructions: 0, cycles: 0, stalls: 0 };
    simulator.stallCounter = 0;
    simulator.hazardInfo = [];
    simulator.forwardingPaths = [];
    
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

// Add example programs
const examples = {
    basic: `# Exemplo básico
addi x1, x0, 10
addi x2, x0, 20
add x3, x1, x2
sub x4, x3, x1
sw x4, 0(x0)
lw x5, 0(x0)`,
    
    hazards: `# Exemplo com hazards
addi x1, x0, 100
addi x2, x1, 50    # RAW hazard (resolvido com forwarding)
lw x3, 0(x0)
add x4, x3, x1     # Load-use hazard (requer stall)
sw x4, 4(x0)`,
    
    branch: `# Exemplo com branch
addi x1, x0, 10
addi x2, x0, 10
beq x1, x2, equal
addi x3, x0, 1
equal:
addi x4, x0, 2`,

    forwarding: `# Exemplo de forwarding
addi x1, x0, 10
addi x2, x1, 5     # Forwarding MEM->EX
add x3, x2, x1     # Forwarding WB->EX e MEM->EX
sub x4, x3, x2     # Forwarding WB->EX`
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