; DebuggerDetect.asm - Assembly Module

section .text
global detect_debugger

detect_debugger:
    ; PTRACE_TRACEME check
    mov eax, 101         ; ptrace syscall
    xor edi, edi         ; PTRACE_TRACEME
    xor esi, esi
    xor edx, edx
    xor r10, r10
    syscall
    cmp rax, -1
    je .debugger_detected
    
    ; CPUID hypervisor bit
    mov eax, 1
    cpuid
    test ecx, 0x80000000
    jnz .vm_detected
    
    ; Timing check
    rdtsc
    mov r8d, eax
    cpuid
    rdtsc
    sub eax, r8d
    cmp eax, 1000
    jg .anomaly_detected
    
    xor eax, eax
    ret

.debugger_detected:
.vm_detected:
.anomaly_detected:
    mov eax, 1
    ret
