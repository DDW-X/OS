section .text

; غیرفعال‌کردن و فعال‌کردن وقفه‌ها
disable_interrupts:
    cli
    ret

enable_interrupts:
    sti
    ret

; دسترسی به رجیسترهای کنترل
read_cr0:
    mov rax, cr0
    ret

write_cr0:
    mov cr0, rdi
    ret

; دسترسی به رجیسترهای مدل خاص (MSR)
read_msr:
    ; rdi = آدرس MSR
    mov rcx, rdi
    rdmsr
    shl rdx, 32
    or rax, rdx
    ret

write_msr:
    ; rdi = آدرس MSR
    ; rsi = مقدار
    mov rcx, rdi
    mov rax, rsi
    shr rsi, 32
    mov rdx, rsi
    wrmsr
    ret

; فعال‌سازی حالت 64 بیتی
enable_long_mode:
    ; فعال‌سازی PAE
    mov eax, cr4
    or eax, CR4_PAE
    mov cr4, eax

    ; تنظیم جدول صفحه‌بندی سطح 4
    mov cr3, PML4_TABLE

    ; فعال‌سازی EFER.LME
    mov ecx, EFER_MSR
    rdmsr
    or eax, EFER_LME
    wrmsr

    ; فعال‌سازی پیجینگ و حفاظت
    mov eax, cr0
    or eax, CR0_PG | CR0_WP
    mov cr0, eax
    ret
    