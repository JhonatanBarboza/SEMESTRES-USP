class Notifications {
    constructor(timer) {
        this.timer = timer;
        this.audio = document.getElementById('alarme');
        this.setup();
    }
    
    setup() {
        // Solicita permissão para notificações
        if ('Notification' in window && Notification.permission !== 'granted') {
            Notification.requestPermission();
        }
    }
    
    playSound() {
        this.audio.currentTime = 0;
        this.audio.play().catch(e => console.log('Erro ao reproduzir som:', e));
    }
    
    showNotification() {
        if ('Notification' in window && Notification.permission === 'granted') {
            const title = 'Tempo concluído!';
            const body = this.timer.mode === 'pomodoro' 
                ? 'Hora de uma pausa!' 
                : 'Hora de voltar ao trabalho!';
            
            new Notification(title, { body });
        }
    }
}

export default Notifications;