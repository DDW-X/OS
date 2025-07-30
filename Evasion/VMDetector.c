// VMDetector.c - C Source File

#include <cpuid.h>
#include <string.h>

int detect_virtualization() {
    unsigned int eax, ebx, ecx, edx;
    
    // Check hypervisor bit
    __get_cpuid(1, &eax, &ebx, &ecx, &edx);
    if(ecx & (1 << 31)) {
        return 1;  // Running in VM
    }
    
    // Check vendor string
    __get_cpuid(0x40000000, &eax, &ebx, &ecx, &edx);
    char vendor[13];
    memcpy(vendor, &ebx, 4);
    memcpy(vendor+4, &ecx, 4);
    memcpy(vendor+8, &edx, 4);
    vendor[12] = 0;
    
    if(strcmp(vendor, "KVMKVMKVM") == 0 ||
       strcmp(vendor, "VMwareVMware") == 0 ||
       strcmp(vendor, "XenVMMXenVMM") == 0) {
        return 1;
    }
    
    // Check for common VM artifacts
    FILE *cpuinfo = fopen("/proc/cpuinfo", "r");
    if(cpuinfo) {
        char line[256];
        while(fgets(line, sizeof(line), cpuinfo) {
            if(strstr(line, "hypervisor") != NULL) {
                fclose(cpuinfo);
                return 1;
            }
        }
        fclose(cpuinfo);
    }
    
    return 0;
}
