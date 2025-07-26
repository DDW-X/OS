// placeholder
#include <stdio.h>
#include <stdlib.h>
#include <dlfcn.h>

#define TEST_MODULE_PATH "build/kernel/deepsick.ko"

int main() {
    void *handle = dlopen(TEST_MODULE_PATH, RTLD_LAZY);
    if (!handle) {
        fprintf(stderr, "Module load failed: %s\n", dlerror());
        return 1;
    }
    
    int (*init_func)() = dlsym(handle, "init_module");
    void (*exit_func)() = dlsym(handle, "cleanup_module");
    
    if (!init_func || !exit_func) {
        fprintf(stderr, "Symbol resolution failed\n");
        dlclose(handle);
        return 1;
    }
    
    if (init_func() != 0) {
        fprintf(stderr, "Module initialization failed\n");
        dlclose(handle);
        return 1;
    }
    
    // Simulate proc write
    int (*proc_write)(const char*, size_t) = dlsym(handle, "proc_write_handler");
    if (proc_write && proc_write("1", 1) == 1) {
        printf("Destruction command successful\n");
    }
    
    exit_func();
    dlclose(handle);
    return 0;
}
