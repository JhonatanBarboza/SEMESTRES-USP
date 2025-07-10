class Timer {
    constructor() {
        this.minutes = 25;
        this.seconds = 0;
        this.isRunning = false;
        this.mode = 'pomodoro';
        this.interval = null;
        this.pomodoroCount = 0;
        
        // Tempos padrão
        this.times = {
            pomodoro: 25,
            shortBreak: 5,
            longBreak: 15
        };
    }
    
    start(callback) {
        if (this.isRunning) return;
        
        this.isRunning = true;
        this.interval = setInterval(() => {
            this.tick();
            if (callback) callback();
        }, 1000);
    }
    
    pause() {
        if (!this.isRunning) return;
        
        clearInterval(this.interval);
        this.isRunning = false;
    }
    
    reset() {
        this.pause();
        this.minutes = this.times[this.mode];
        this.seconds = 0;
    }
    
    tick() {
        if (this.seconds === 0) {
            if (this.minutes === 0) {
                this.complete();
                return;
            }
            this.minutes--;
            this.seconds = 59;
        } else {
            this.seconds--;
        }
    }
    
    complete() {
        this.pause();
        if (this.mode === 'pomodoro') {
            this.pomodoroCount++;
        }
        return true;
    }
    
    setMode(mode) {
        this.mode = mode;
        this.reset();
    }
    
    getFormattedTime() {
        return `${this.minutes.toString().padStart(2, '0')}:${this.seconds.toString().padStart(2, '0')}`;
    }
    
    updateTimes(settings) {
        this.times.pomodoro = settings.pomodoro;
        this.times.shortBreak = settings.shortBreak;
        this.times.longBreak = settings.longBreak;
        
        if (!this.isRunning) {
            this.reset();
        }
    }
}

export default Timer;