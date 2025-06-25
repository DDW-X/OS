section .text

; بازیابی کد تخریب شده
restore_modified_code:
    ; بررسی صحت CRC
    call calculate_code_crc
    cmp eax, [expected_crc]
    je .no_corruption

    ; بازیابی از نسخه پشتیبان
    mov rsi, [code_backup]
    mov rdi, [code_start]
    mov rcx, [code_size]
    rep movsb

    ; تغییر الگوریتم مبهم‌سازی
    call change_obfuscation_scheme

.no_corruption:
    ret

; سیستم نظارت مداوم
continuous_monitoring:
    ; ایجاد تایمر
    call setup_integrity_timer

    ; فعال‌سازی محافظت از حافظه
    call enable_memory_guards
    ret

; تابع تایمر برای بررسی صحت
integrity_timer_callback:
    ; بررسی صحت کد حیاتی
    mov rdi, [critical_code_start]
    mov rsi, [critical_code_size]
    call calculate_crc
    cmp eax, [expected_critical_crc]
    jne .corruption_detected

    ; بررسی هوک‌ها
    call detect_hooks
    test eax, eax
    jnz .hook_detected

    ret

.corruption_detected:
    call restore_critical_code
    ret

.hook_detected:
    call remove_hooks
    ret
    