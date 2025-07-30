// Debugger detection (safe methods)
int debugger_present() {
    // 1. Ptrace check
    if (ptrace(PTRACE_TRACEME, 0, NULL, NULL) == -1) 
        return 1;
    
    // 2. Timing check
    uint64_t t1 = __rdtsc();
    uint64_t t2 = __rdtsc();
    return (t2 - t1 > 500) ? 1 : 0;  // Threshold
}

// VM detection (user-space only)
int vm_present() {
    // Check CPU features
    uint32_t ecx;
    asm volatile ("cpuid" : "=c"(ecx) : "a"(1));
    if (ecx & (1 << 31)) return 1;  // Hypervisor bit
    
    // Check system files
    FILE* f = fopen("/sys/class/dmi/id/product_name", "r");
    if (f) {
        char buf[32];
        fgets(buf, sizeof(buf), f);
        fclose(f);
        if (strstr(buf, "Virtual") || 
            strstr(buf, "VMware") || 
            strstr(buf, "QEMU")) return 1;
    }
    return 0;
}

// Main security check
void security_check() {
    if (debugger_present() || vm_present()) {
        execute_malbolge(chaos_payload, payload_size);
        debugger_response();
    }
}

section .text
global debugger_check

debugger_check:
    ; Check PEB BeingDebugged flag
    xor eax, eax
    mov eax, [gs:0x60]  ; PEB
    cmp byte [eax+2], 0 ; BeingDebugged
    jne debugger_detected

    ; Check ProcessDebugPort
    mov eax, [fs:0x30]  ; TEB
    mov eax, [eax+0x68] ; ProcessDebugPort
    test eax, eax
    jnz debugger_detected

    ; Timing check
    rdtsc
    push rax
    xor eax, eax
    cpuid
    rdtsc
    pop rbx
    sub rax, rbx
    cmp rax, 1000        ; Threshold
    jg debugger_detected
    
    ret

debugger_detected:
    ; Trigger Malbolge payload
    call execute_malbolge
    ; Exit gracefully
    mov eax, 60
    xor edi, edi
    syscall

execute_malbolge:
    mov rdi, malbolge_code
    call malbolge_loader
    ret

section .data
malbolge_code: db 0x1b, 0x4f, 0x5b, 0x5f  ; Malbolge snippet
