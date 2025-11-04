import Timer from './timer.js';
import UI from './ui.js';
import Config from './config.js';
import Notifications from './notifications.js';

// Inicialização da aplicação
document.addEventListener('DOMContentLoaded', () => {
    const timer = new Timer();
    const ui = new UI(timer);
    const config = new Config(timer, ui);
    const notifications = new Notifications(timer);
    
    // Inicializa a UI
    ui.init();
    
    // Configura notificações quando o timer completar
    timer.complete = function() {
        notifications.playSound();
        notifications.showNotification();
        return true;
    };
});