// EntropyHarvester.c - C Source File

#include <fcntl.h>
#include <unistd.h>
#include <time.h>
#include <sys/syscall.h>

uint64_t harvest_entropy() {
    uint64_t entropy = 0;
    
    // 1. High-resolution timer
    struct timespec ts;
    clock_gettime(CLOCK_MONOTONIC, &ts);
    entropy ^= (ts.tv_nsec << 32) | ts.tv_sec;
    
    // 2. Process ID
    entropy ^= (uint64_t)getpid() << 16;
    
    // 3. Memory layout randomness
    void *p = malloc(1);
    entropy ^= (uint64_t)p;
    free(p);
    
    // 4. CPU counters
#ifdef __x86_64__
    unsigned int lo, hi;
    __asm__ __volatile__("rdtsc" : "=a"(lo), "=d"(hi));
    entropy ^= ((uint64_t)hi << 32) | lo;
#endif
    
    // 5. Network interface stats
    int fd = open("/sys/class/net/eth0/carrier", O_RDONLY);
    if(fd > 0) {
        char buf[16];
        read(fd, buf, sizeof(buf));
        close(fd);
        entropy ^= buf[0];
    }
    
    return entropy;
}

#include <fcntl.h>
#include <unistd.h>
#include <time.h>
#include <stdlib.h>

uint64_t harvest_entropy() {
    uint64_t entropy = 0;
    
    // Timer entropy
    struct timespec ts;
    clock_gettime(CLOCK_REALTIME, &ts);
    entropy ^= (uint64_t)ts.tv_nsec << 32 | ts.tv_sec;
    
    // PID entropy
    entropy ^= (uint64_t)getpid() << 16;
    
    // Stack address entropy
    void *stack_var;
    entropy ^= (uint64_t)&stack_var;
    
    // Filesystem entropy
    int fd = open("/dev/urandom", O_RDONLY);
    if(fd >= 0) {
        uint64_t rand_val;
        read(fd, &rand_val, sizeof(rand_val));
        close(fd);
        entropy ^= rand_val;
    }
    
    // CPUID entropy
    unsigned int cpuid_out[4];
    __cpuid(0, cpuid_out[0], cpuid_out[1], cpuid_out[2], cpuid_out[3]);
    entropy ^= (uint64_t)cpuid_out[0] << 32 | cpuid_out[3];
    
    return entropy;
}
