section .text
global init_quantum_rng, get_quantum_random

%include "quantum_rng.inc"

init_quantum_rng:
    ; باز کردن دستگاه کوانتومی
    mov rax, SYS_open
    mov rdi, quantum_device
    mov rsi, O_RDWR
    syscall
    mov [quantum_fd], rax
    
    ; تنظیم پارامترهای کوانتومی
    mov rdi, rax
    mov rsi, QUANTUM_INIT
    mov rdx, quantum_params
    mov rax, SYS_ioctl
    syscall
    
    ; تولید seed اولیه
    call generate_quantum_seed
    ret

generate_quantum_seed:
    ; خواندن داده تصادفی از دستگاه کوانتومی
    mov rdi, [quantum_fd]
    mov rsi, quantum_buffer
    mov rdx, QUANTUM_SEED_SIZE
    mov rax, SYS_read
    syscall
    
    ; تبدیل به seed رمزنگاری
    mov rdi, quantum_buffer
    mov rsi, rax
    call hash_to_seed
    mov [quantum_seed], rax
    ret

get_quantum_random:
    ; تولید عدد تصادفی کوانتومی
    mov rdi, [quantum_fd]
    mov rsi, QUANTUM_GET_RANDOM
    mov rdx, quantum_random_buffer
    mov rax, SYS_ioctl
    syscall
    
    ; بازگرداندن مقدار
    mov rax, [quantum_random_buffer]
    ret
    