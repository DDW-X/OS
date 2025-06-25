section .text

; یافتن جدول ACPI
find_acpi_table:
    ; rdi = امضای جدول (4 کاراکتر)
    
    ; بررسی RSDT/XSDT
    mov rax, [RSDP]
    test rax, rax
    jz .not_found
    
    ; بررسی نسخه ACPI
    cmp byte [rax + 15], 2
    jae .use_xsdt
    
    ; استفاده از RSDT
    mov rsi, [rax + 16] ; آدرس RSDT
    jmp .search_table
    
.use_xsdt:
    ; استفاده از XSDT
    mov rsi, [rax + 24] ; آدرس XSDT
    
.search_table:
    ; تعداد ورودی‌ها
    mov ecx, [rsi + 4]
    sub ecx, 36
    shr ecx, (4 - 1) ; تقسیم بر 4 یا 8
    
    ; حلقه جستجو
    add rsi, 36
.search_loop:
    ; مقایسه امضا
    mov eax, [rsi]
    cmp eax, edi
    je .found
    
    ; ورودی بعدی
    add rsi, (4 + 4) ; برای RSDT و XSDT
    loop .search_loop
    
.not_found:
    xor rax, rax
    ret
    
.found:
    mov rax, [rsi] ; آدرس جدول
    ret

; تغییر جدول DSDT
modify_dsdt:
    ; rdi = آدرس DSDT
    ; rsi = کد AML جدید
    ; rdx = اندازه کد جدید
    
    ; یافتن محل تزریق
    mov rcx, [rdi + 4] ; طول جدول
    sub rcx, 36
    
.injection_search:
    mov al, [rdi + rcx]
    cmp al, AML_RETURN_OP
    je .found_location
    dec rcx
    jnz .injection_search
    
.found_location:
    ; تزریق کد جدید
    lea rdi, [rdi + rcx]
    mov rcx, rdx
    rep movsb
    
    ; به‌روزرسانی چک‌سام
    call update_dsdt_checksum
    ret
    