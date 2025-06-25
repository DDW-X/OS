section .text

; تشخیص محیط‌های مجازی پیشرفته
advanced_vm_detection:
    ; تکنیک CPUID leaf
    mov eax, 0x40000000
    cpuid
    cmp ebx, 0x40000000  ; Hypervisor vendor
    jae .vm_detected

    ; تکنیک I/O port
    mov dx, 0x5658  ; VMware magic port
    in eax, dx
    cmp eax, 0x564D5868  ; 'VMXh'
    je .vm_detected

    ; تکنیک timing
    call timing_vm_detection
    test eax, eax
    jnz .vm_detected

    ; تکنیک BIOS
    call check_bios_signature
    test eax, eax
    jnz .vm_detected

    ret

.vm_detected:
    call vm_response
    ret

; تشخیص مبتنی بر زمان
timing_vm_detection:
    rdtsc
    mov [start_tsc], eax
    mov [start_tsc+4], edx

    ; اجرای دستورات سنگین
    mov ecx, 10000
.loop:
    rdrand eax
    bswap eax
    crc32 eax, ebx
    loop .loop

    rdtsc
    sub eax, [start_tsc]
    sbb edx, [start_tsc+4]

    ; بررسی آستانه زمانی
    cmp edx, 0
    ja .vm_suspected
    cmp eax, 500000  ; 500,000 cycles
    ja .vm_suspected

    xor eax, eax
    ret

.vm_suspected:
    mov eax, 1
    ret

; پاسخ به تشخیص VM
vm_response:
    ; فعال‌سازی رفتار فریبنده
    call activate_vm_decoy
    
    ; غیرفعال‌سازی ویژگی‌های حساس
    call disable_vm_sensitive_features
    
    ; شبیه‌سازی رفتار عادی
    call simulate_normal_behavior
    ret
    