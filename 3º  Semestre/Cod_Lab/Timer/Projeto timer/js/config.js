class Config {
    constructor(timer, ui) {
        this.timer = timer;
        this.ui = ui;
        this.elements = {
            pomodoroInput: document.getElementById('pomodoro'),
            shortBreakInput: document.getElementById('pausa-curta'),
            longBreakInput: document.getElementById('pausa-longa'),
            settingsSection: document.getElementById('configuracoes')
        };
        
        this.loadSettings();
    }
    
    loadSettings() {
        const settings = {
            pomodoro: localStorage.getItem('pomodoroTime') || 25,
            shortBreak: localStorage.getItem('shortBreakTime') || 5,
            longBreak: localStorage.getItem('longBreakTime') || 15
        };
        
        // Atualiza inputs
        this.elements.pomodoroInput.value = settings.pomodoro;
        this.elements.shortBreakInput.value = settings.shortBreak;
        this.elements.longBreakInput.value = settings.longBreak;
        
        // Atualiza timer
        this.timer.updateTimes(settings);
        this.ui.updateDisplay();
        
        // Configura listeners
        this.setupRealTimeListeners();
    }
    
    setupRealTimeListeners() {
        // Observa mudanças em tempo real nos inputs
        Object.values(this.elements).forEach(input => {
            if (input.tagName === 'INPUT') {
                input.addEventListener('input', () => {
                    if (!this.validateInput(input)) return;
                    
                    this.saveSettings();
                    
                    if (!this.timer.isRunning) {
                        this.updateTimerForCurrentMode();
                    }
                });
            }
        });
    }
    
    validateInput(input) {
        const value = parseInt(input.value);
        const min = parseInt(input.min);
        const max = parseInt(input.max);
        
        if (isNaN(value) || value < min || value > max) {
            input.classList.add('error');
            
            // Cria ou atualiza mensagem de erro
            let errorMsg = input.nextElementSibling;
            if (!errorMsg || !errorMsg.classList.contains('error-message')) {
                errorMsg = document.createElement('div');
                errorMsg.className = 'error-message';
                input.parentNode.insertBefore(errorMsg, input.nextSibling);
            }
            
            errorMsg.textContent = `Digite um valor entre ${min} e ${max}`;
            return false;
        }
        
        input.classList.remove('error');
        if (input.nextElementSibling?.classList.contains('error-message')) {
            input.nextElementSibling.remove();
        }
        return true;
    }
    
    saveSettings() {
        const settings = {
            pomodoro: parseInt(this.elements.pomodoroInput.value),
            shortBreak: parseInt(this.elements.shortBreakInput.value),
            longBreak: parseInt(this.elements.longBreakInput.value)
        };
        
        // Validação adicional
        if (settings.pomodoro < 1 || settings.shortBreak < 1 || settings.longBreak < 1) {
            return;
        }
        
        // Salva no localStorage
        localStorage.setItem('pomodoroTime', settings.pomodoro);
        localStorage.setItem('shortBreakTime', settings.shortBreak);
        localStorage.setItem('longBreakTime', settings.longBreak);
        
        // Atualiza timer
        this.timer.updateTimes(settings);
        this.ui.updateDisplay();
    }
    
    updateTimerForCurrentMode() {
        const mode = this.timer.mode;
        let newTime;
        
        switch(mode) {
            case 'pomodoro':
                newTime = parseInt(this.elements.pomodoroInput.value);
                break;
            case 'pausa-curta':
                newTime = parseInt(this.elements.shortBreakInput.value);
                break;
            case 'pausa-longa':
                newTime = parseInt(this.elements.longBreakInput.value);
                break;
        }
        
        if (!isNaN(newTime)) {
            this.timer.minutes = newTime;
            this.timer.seconds = 0;
            this.ui.updateDisplay();
        }
    }
}

export default Config;