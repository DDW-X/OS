section .text

; ورود به SMM
enter_smm:
    ; ذخیره وضعیت فعلی
    pushaq
    
    ; فعال‌سازی SMI
    mov dx, SMI_TRIGGER_PORT
    in al, dx
    or al, SMI_TRIGGER_BIT
    out dx, al
    
    ; انتظار برای بازگشت
.smm_wait:
    pause
    jmp .smm_wait
    
    ; اینجا اجرا نمی‌شود
    popaq
    ret

; تنظیم هندلر SMI سفارشی
install_smi_handler:
    ; rdi = آدرس هندلر
    
    ; غیرفعال‌کردن SMI
    cli
    
    ; دریافت آدرس SMRAM
    mov eax, SMRAM_BASE_MSR
    rdmsr
    mov [smram_base], eax
    
    ; دستکاری جدول هندلر SMI
    mov rsi, [smram_base]
    add rsi, SMI_HANDLER_TABLE_OFFSET
    mov [rsi], rdi
    
    ; فعال‌کردن مجدد SMI
    sti
    ret
    