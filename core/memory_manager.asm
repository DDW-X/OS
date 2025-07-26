section .text

; فعال‌سازی دسترسی مستقیم به حافظه فیزیکی
enable_physical_memory_access:
    ; غیرفعال‌کردن حفاظت حافظه
    mov rax, cr0
    and rax, 0xFFFEFFFF    ; clear WP bit
    mov cr0, rax
    
    ; فعال‌سازی دسترسی به تمام حافظه
    mov rax, cr4
    or rax, CR4_PAE | CR4_PSE  ; فعال‌سازی PAE و PSE
    mov cr4, rax
    
    ; تنظیم جدول صفحه‌بندی سفارشی
    mov cr3, custom_page_table
    ret

; نگاشت حافظه فیزیکی به فضای مجازی
map_physical_to_virtual:
    ; rdi = آدرس فیزیکی
    ; rsi = اندازه
    ; rdx = آدرس مجازی هدف
    
    mov rcx, rsi
    shr rcx, 12           ; تعداد صفحات
    mov r8, rdi
    mov r9, rdx
    
.map_loop:
    ; ساخت ورودی صفحه
    mov rax, r8
    or rax, PAGE_PRESENT | PAGE_RW | PAGE_GLOBAL
    mov [r9], rax
    
    add r8, 0x1000        ; صفحه بعدی فیزیکی
    add r9, 8             ; ورودی بعدی جدول صفحه
    loop .map_loop
    
    ; بازگردانی TLB
    invlpg [rdx]
    ret

; اختصاص حافظه هسته سطح پایین
allocate_low_memory:
    ; rdi = اندازه
    mov rax, 0xFFFFF000   ; آخرین صفحه حافظه پایین
    sub rax, rdi
    and rax, 0xFFFFF000   ; تراز صفحه‌ای
    mov [low_mem_ptr], rax
    ret
    