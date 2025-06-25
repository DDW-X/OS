section .text

; پاک‌کردن رجیسترهای دیباگ
clear_debug_registers:
    xor eax, eax
    mov dr0, eax
    mov dr1, eax
    mov dr2, eax
    mov dr3, eax
    mov dr6, eax
    mov dr7, eax
    ret

; تشخیص استفاده از رجیسترهای دیباگ
detect_debug_registers_use:
    mov eax, dr0
    test eax, eax
    jnz .debugger_detected
    mov eax, dr1
    test eax, eax
    jnz .debugger_detected
    mov eax, dr2
    test eax, eax
    jnz .debugger_detected
    mov eax, dr3
    test eax, eax
    jnz .debugger_detected
    ret

.debugger_detected:
    call debug_registers_response
    ret

; پاسخ به تشخیص استفاده از رجیسترها
debug_registers_response:
    ; تنظیم تله‌های جعلی
    call set_fake_hardware_breakpoints
    
    ; تغییر مقادیر رجیسترها
    rdrand eax
    mov dr0, eax
    rdrand eax
    mov dr1, eax
    rdrand eax
    mov dr2, eax
    rdrand eax
    mov dr3, eax
    
    ; فعال‌سازی DR7 با تنظیمات اشتباه
    mov eax, 0x00000400  ; تنظیمات اشتباه
    mov dr7, eax
    ret
    