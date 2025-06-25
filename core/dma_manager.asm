section .text

; پیکربندی کانال DMA
setup_dma_channel:
    ; rdi = کانال (0-7)
    ; rsi = حالت
    ; rdx = آدرس فیزیکی بافر
    ; rcx = طول
    
    ; غیرفعال‌کردن کانال
    mov al, dil
    or al, DMA_MASK_BIT
    out DMA_MASK_REG, al
    
    ; پاک‌کردن بیت‌های قبلی
    mov al, DMA_CLEAR_BYTE
    out DMA_CLEAR_REG, al
    
    ; تنظیم حالت
    mov al, sil
    or al, dil
    out DMA_MODE_REG, al
    
    ; تنظیم آدرس
    mov rax, rdx
    out DMA_ADDR_REG + (rdi * 2), al
    shr rax, 8
    out DMA_ADDR_REG + (rdi * 2), al
    
    ; تنظیم طول
    mov rax, rcx
    out DMA_COUNT_REG + (rdi * 2), al
    shr rax, 8
    out DMA_COUNT_REG + (rdi * 2), al
    
    ; فعال‌کردن کانال
    mov al, dil
    out DMA_MASK_REG, al
    ret

; شروع انتقال DMA
start_dma_transfer:
    ; rdi = کانال
    mov al, dil
    out DMA_START_REG, al
    ret
    