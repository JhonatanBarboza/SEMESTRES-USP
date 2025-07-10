document.addEventListener('DOMContentLoaded', function() {
    // Elementos da interface
    const assemblyCode = document.getElementById('assembly-code');
    const runBtn = document.getElementById('run-btn');
    const stepBtn = document.getElementById('step-btn');
    const resetBtn = document.getElementById('reset-btn');
    const forwardingToggle = document.getElementById('forwarding-toggle');
    const hazardToggle = document.getElementById('hazard-toggle');
    const pipelineVisualization = document.getElementById('pipeline-visualization');
    const hazardInfo = document.getElementById('hazard-info');
    const registersDiv = document.getElementById('registers');
    const memoryDiv = document.getElementById('memory');
    
    // Estado do simulador
    let simulator = new PipelineSimulator();
    let isRunning = false;
    let stepInterval = null;
    
    // Configuração dos botões
    runBtn.addEventListener('click', function() {
        if (isRunning) {
            stopSimulation();
            runBtn.textContent = 'Executar';
        } else {
            startSimulation();
            runBtn.textContent = 'Parar';
        }
    });
    
    stepBtn.addEventListener('click', function() {
        if (!isRunning) {
            simulator.step();
            updateUI();
        }
    });
    
    resetBtn.addEventListener('click', function() {
        stopSimulation();
        simulator.reset();
        simulator.loadCode(assemblyCode.value);
        updateUI();
        runBtn.textContent = 'Executar';
    });
    
    forwardingToggle.addEventListener('click', function() {
        simulator.forwardingEnabled = !simulator.forwardingEnabled;
        forwardingToggle.textContent = `Forwarding: ${simulator.forwardingEnabled ? 'Ativado' : 'Desativado'}`;
    });
    
    hazardToggle.addEventListener('click', function() {
        simulator.hazardDetectionEnabled = !simulator.hazardDetectionEnabled;
        hazardToggle.textContent = `Stalls: ${simulator.hazardDetectionEnabled ? 'Ativado' : 'Desativado'}`;
    });
    
    // Funções de controle da simulação
    function startSimulation() {
        if (!simulator.isLoaded()) {
            simulator.loadCode(assemblyCode.value);
        }
        
        isRunning = true;
        stepInterval = setInterval(function() {
            if (simulator.isFinished()) {
                stopSimulation();
                runBtn.textContent = 'Executar';
                return;
            }
            
            simulator.step();
            updateUI();
        }, 1000);
    }
    
    function stopSimulation() {
        isRunning = false;
        if (stepInterval) {
            clearInterval(stepInterval);
            stepInterval = null;
        }
    }
    
    // Atualiza a interface
    function updateUI() {
        updatePipelineVisualization();
        updateRegisters();
        updateMemory();
        updateStats();
        updateHazardInfo();
    }
    
    function updatePipelineVisualization() {
        pipelineVisualization.innerHTML = '';
        
        // Cabeçalho com os estágios
        const header = document.createElement('div');
        header.className = 'pipeline-row';
        
        const emptyLabel = document.createElement('div');
        emptyLabel.className = 'cycle-label';
        emptyLabel.textContent = 'Ciclo';
        header.appendChild(emptyLabel);
        
        ['IF', 'ID', 'EX', 'MEM', 'WB'].forEach(stage => {
            const stageLabel = document.createElement('div');
            stageLabel.className = 'pipeline-stage';
            stageLabel.textContent = stage;
            header.appendChild(stageLabel);
        });
        
        pipelineVisualization.appendChild(header);
        
        // Linhas para cada ciclo
        const history = simulator.getPipelineHistory();
        for (let cycle = 0; cycle < history.length; cycle++) {
            const row = document.createElement('div');
            row.className = 'pipeline-row';
            
            const cycleLabel = document.createElement('div');
            cycleLabel.className = 'cycle-label';
            cycleLabel.textContent = cycle;
            row.appendChild(cycleLabel);
            
            const stages = history[cycle];
            for (let i = 0; i < 5; i++) {
                const stage = document.createElement('div');
                stage.className = 'pipeline-stage';
                
                if (stages[i]) {
                    stage.textContent = stages[i].instruction || '';
                    stage.title = stages[i].instruction || '';
                    
                    if (stages[i].active) {
                        stage.classList.add('active');
                    }
                    if (stages[i].stalled) {
                        stage.classList.add('stalled');
                    }
                    if (stages[i].hazard) {
                        stage.classList.add('hazard');
                    }
                }
                
                row.appendChild(stage);
            }
            
            pipelineVisualization.appendChild(row);
        }
    }
    
    function updateRegisters() {
        const registers = simulator.getRegisters();
        registersDiv.innerHTML = '';
        
        for (let i = 0; i < 32; i++) {
            const regDiv = document.createElement('div');
            regDiv.className = 'register';
            
            const nameSpan = document.createElement('span');
            nameSpan.className = 'register-name';
            nameSpan.textContent = `x${i}`;
            
            const valueSpan = document.createElement('span');
            valueSpan.className = 'register-value';
            valueSpan.textContent = registers[i];
            
            regDiv.appendChild(nameSpan);
            regDiv.appendChild(valueSpan);
            registersDiv.appendChild(regDiv);
        }
    }
    
    function updateMemory() {
        const memory = simulator.getMemory();
        memoryDiv.innerHTML = '';
        
        for (let addr in memory) {
            if (memory.hasOwnProperty(addr)) {
                const memDiv = document.createElement('div');
                memDiv.className = 'memory-cell';
                
                const addrSpan = document.createElement('span');
                addrSpan.className = 'memory-address';
                addrSpan.textContent = `0x${parseInt(addr).toString(16)}`;
                
                const valueSpan = document.createElement('span');
                valueSpan.className = 'memory-value';
                valueSpan.textContent = memory[addr];
                
                memDiv.appendChild(addrSpan);
                memDiv.appendChild(valueSpan);
                memoryDiv.appendChild(memDiv);
            }
        }
    }
    
    function updateStats() {
        document.getElementById('total-cycles').textContent = simulator.getCycleCount();
        document.getElementById('completed-instructions').textContent = simulator.getCompletedInstructions();
        document.getElementById('cpi').textContent = (simulator.getCycleCount() / simulator.getCompletedInstructions()).toFixed(2);
        document.getElementById('stall-count').textContent = simulator.getStallCount();
        document.getElementById('forwarding-count').textContent = simulator.getForwardingCount();
    }
    
    function updateHazardInfo() {
        const hazards = simulator.getCurrentHazards();
        hazardInfo.innerHTML = '';
        
        if (hazards.length === 0) {
            return;
        }
        
        const hazardTitle = document.createElement('h3');
        hazardTitle.textContent = 'Hazards Detectados:';
        hazardInfo.appendChild(hazardTitle);
        
        hazards.forEach(hazard => {
            const hazardDiv = document.createElement('div');
            hazardDiv.className = 'hazard-info';
            
            const typeSpan = document.createElement('span');
            typeSpan.textContent = `Tipo: ${hazard.type}`;
            
            const instructionSpan = document.createElement('span');
            instructionSpan.textContent = `Instrução: ${hazard.instruction}`;
            
            const actionSpan = document.createElement('span');
            actionSpan.textContent = `Ação: ${hazard.action}`;
            
            hazardDiv.appendChild(typeSpan);
            hazardDiv.appendChild(document.createElement('br'));
            hazardDiv.appendChild(instructionSpan);
            hazardDiv.appendChild(document.createElement('br'));
            hazardDiv.appendChild(actionSpan);
            
            hazardInfo.appendChild(hazardDiv);
        });
    }
    
    // Inicialização
    simulator.loadCode(assemblyCode.value);
    updateUI();
});