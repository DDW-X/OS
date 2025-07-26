section .text
global ftrace_hook_function

%include "ftrace.inc"

ftrace_hook_function:
    ; RDI = آدرس تابع هدف
    ; RSI = آدرس تابع جایگزین
    
    ; غیرفعال‌سازی حفاظت‌ها
    call disable_memory_protections
    
    ; جستجوی ftrace_ops مربوطه
    mov rax, [ftrace_ops_list]
    .find_loop:
        test rax, rax
        jz .not_found
        mov rbx, [rax + FTRACE_OPS_FUNC]
        cmp rbx, rdi
        je .found
        mov rax, [rax + FTRACE_OPS_NEXT]
        jmp .find_loop
    
    .found:
        ; جایگزینی تابع
        mov [rax + FTRACE_OPS_FUNC], rsi
        
        ; فعال‌سازی مجدد ftrace
        mov rdi, rax
        call ftrace_activate
    
    .not_found:
        ; بازگردانی حفاظت‌ها
        call enable_memory_protections
        ret

ftrace_activate:
    ; فعال‌سازی ftrace ops
    mov rdx, [rdi + FTRACE_OPS_FLAGS]
    or rdx, FTRACE_OPS_FL_ENABLED
    mov [rdi + FTRACE_OPS_FLAGS], rdx
    
    ; اعمال تغییرات
    mov rax, __fentry__
    call rdi
    ret
    