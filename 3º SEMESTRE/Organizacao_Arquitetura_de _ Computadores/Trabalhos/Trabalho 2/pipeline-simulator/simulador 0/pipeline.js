// Instruções de exemplo (RISC-V assembly)
const instructions = [
    { text: "ADD X1, X2, X3",   type: "arith",   rs1: "X2", rs2: "X3", rd: "X1" },
    { text: "SUB X4, X1, X5",   type: "arith",   rs1: "X1", rs2: "X5", rd: "X4" }, // Hazard de dados!
    { text: "LW X6, 0(X1)",     type: "load",    rs1: "X1", imm: "0",  rd: "X6" },
    { text: "BEQ X1, X0, LABEL", type: "branch", rs1: "X1", rs2: "X0", offset: "LABEL" } // Hazard de controle!
];

let cycle = 0;
let pipeline = [];
const mode = document.getElementById("mode");
const explanation = document.getElementById("explanation");

// Avança um ciclo no pipeline
document.getElementById("step").addEventListener("click", () => {
    cycle++;
    updatePipeline();
});

// Reinicia o simulador
document.getElementById("reset").addEventListener("click", () => {
    cycle = 0;
    pipeline = [];
    updatePipeline();
});

// Atualiza a visualização
function updatePipeline() {
  const container = document.getElementById("pipeline-container");
  container.innerHTML = '';
  
  // Cria o cabeçalho com os nomes das fases
  const header = document.createElement("div");
  header.className = "pipeline-header";
  
  const stageNames = ["FETCH", "DECODE", "EXECUTE", "MEMORY", "WRITE BACK"];
  stageNames.forEach(name => {
    const stageNameElement = document.createElement("div");
    stageNameElement.className = "stage-name";
    stageNameElement.textContent = name;
    header.appendChild(stageNameElement);
});

container.appendChild(header);
  
  // Preenche o pipeline até o ciclo atual
  for (let c = 0; c < cycle; c++) {
    const pipelineRow = document.createElement("div");
    pipelineRow.className = "pipeline-row";
    
    // Adiciona número da instrução
    const instrNumber = document.createElement("div");
    instrNumber.className = "instruction-number";
    instrNumber.textContent = c < instructions.length ? `${c + 1}.` : '';
    pipelineRow.appendChild(instrNumber);
    
    for (let s = 0; s < 5; s++) {
      const instrPos = c - s;
      const stage = document.createElement("div");
      const stageLabel = document.createElement("div");
      const stageContent = document.createElement("div");
      
      stageLabel.className = "stage-label";
      stageContent.className = "stage-content";
      
      // Define a classe do estágio
      const stageClasses = ["fetch", "decode", "execute", "memory", "writeback"];
      stage.classList.add("stage", stageClasses[s]);
      
      // Define o conteúdo
      if (instrPos >= 0 && instrPos < instructions.length) {
        stageLabel.textContent = stageNames[s];
        stageContent.textContent = instructions[instrPos].text;
        
        // Detecta hazards (simplificado)
        if (instrPos > 0 && hasDataHazard(instrPos)) {
          stage.classList.add("hazard");
          explanation.textContent = "Hazard de dados detectado! A instrução precisa do resultado da anterior.";
        }
      } else if (instrPos >= 0) {
        stageLabel.textContent = stageNames[s];
        stageContent.textContent = "";
      }
      
      stage.appendChild(stageLabel);
      stage.appendChild(stageContent);
      pipelineRow.appendChild(stage);
    }
    
    container.appendChild(pipelineRow);
  }
}

// Verifica hazards de dados (simplificado)
function hasDataHazard(instrPos) {
    const currInstr = instructions[instrPos];
    const prevInstr = instructions[instrPos - 1];
    
    // Hazard se a instrução atual lê um registrador que a anterior escreve
    return (currInstr.rs1 === prevInstr.rd || currInstr.rs2 === prevInstr.rd);
}