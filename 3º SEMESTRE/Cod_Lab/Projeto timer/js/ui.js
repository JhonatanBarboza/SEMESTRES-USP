class UI {
    constructor(timer) {
        this.timer = timer;
        this.elements = {
            display: document.getElementById('cronometro'),
            startPauseBtn: document.getElementById('inicia-pausa'),
            resetBtn: document.getElementById('reset-timer'),
            modeBtns: document.querySelectorAll('.mode-btn'),
            settingsBtn: document.getElementById('mostra-config'),
            settingsSection: document.getElementById('configuracoes'),
            pomodoroCount: document.getElementById('contador-numero')
        };
    }
    
    init() {
        this.updateDisplay();
        this.setupEventListeners();
    }
    
    setupEventListeners() {
        // Botão Iniciar/Pausar
        this.elements.startPauseBtn.addEventListener('click', () => {
            if (this.timer.isRunning) {
                this.timer.pause();
                this.elements.startPauseBtn.innerHTML = '<i class="fas fa-play"></i> Iniciar';
            } else {
                this.timer.start(() => this.updateDisplay());
                this.elements.startPauseBtn.innerHTML = '<i class="fas fa-pause"></i> Pausar';
            }
        });
        
        // Botão Reset
        this.elements.resetBtn.addEventListener('click', () => {
            // Adiciona efeito visual
            this.elements.display.classList.add('resetting');
            
            // Remove a classe após a animação
            setTimeout(() => {
                this.elements.display.classList.remove('resetting');
            }, 500);
            
            this.timer.reset();
            this.updateDisplay();
            
            if (!this.timer.isRunning) {
                this.elements.startPauseBtn.innerHTML = '<i class="fas fa-play"></i> Iniciar';
            }
        });
        
        // Botões de modo
        this.elements.modeBtns.forEach(btn => {
            btn.addEventListener('click', () => {
                const mode = btn.dataset.mode;
                this.setActiveMode(mode);
                this.timer.setMode(mode);
                this.updateDisplay();
            });
        });
        
        // Botão de configurações
        this.elements.settingsBtn.addEventListener('click', () => {
            const isShowing = this.elements.settingsSection.style.display === 'block';
            
            if (isShowing) {
                this.elements.settingsSection.style.display = 'none';
                this.elements.settingsBtn.innerHTML = '<i class="fas fa-cog"></i> Configurações';
            } else {
                this.elements.settingsSection.style.display = 'block';
                this.elements.settingsBtn.innerHTML = '<i class="fas fa-times"></i> Fechar';
            }
        });
    }
    
    updateDisplay() {
        this.elements.display.textContent = this.timer.getFormattedTime();
        this.elements.pomodoroCount.textContent = this.timer.pomodoroCount;
    }
    
    setActiveMode(mode) {
        this.elements.modeBtns.forEach(btn => {
            if (btn.dataset.mode === mode) {
                btn.classList.add('active');
            } else {
                btn.classList.remove('active');
            }
        });
    }
}

export default UI;