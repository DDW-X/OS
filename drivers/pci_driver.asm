section .text

; خواندن از پیکربندی PCI
pci_config_read:
    ; rdi = bus
    ; rsi = device
    ; rdx = function
    ; rcx = offset
    
    ; ساخت آدرس PCI
    mov rax, rdi
    shl rax, 16
    mov ax, si
    shl rax, 8
    or rax, rdx
    shl rax, 8
    or rax, rcx
    or rax, PCI_CONFIG_ENABLE
    
    ; ارسال آدرس
    mov dx, PCI_CONFIG_ADDRESS
    out dx, eax
    
    ; خواندن داده
    mov dx, PCI_CONFIG_DATA
    in eax, dx
    ret

; نوشتن در پیکربندی PCI
pci_config_write:
    ; rdi = bus
    ; rsi = device
    ; rdx = function
    ; rcx = offset
    ; r8 = داده
    
    ; ساخت آدرس PCI
    mov rax, rdi
    shl rax, 16
    mov ax, si
    shl rax, 8
    or rax, rdx
    shl rax, 8
    or rax, rcx
    or rax, PCI_CONFIG_ENABLE
    
    ; ارسال آدرس
    mov dx, PCI_CONFIG_ADDRESS
    out dx, eax
    
    ; نوشتن داده
    mov rax, r8
    mov dx, PCI_CONFIG_DATA
    out dx, eax
    ret

; یافتن دستگاه PCI با شناسه
pci_find_device:
    ; rdi = vendor_id
    ; rsi = device_id
    
    mov rbx, 0    ; bus
.bus_loop:
    mov rcx, 0    ; device
.device_loop:
    ; خواندن شناسه فروشنده
    mov rdx, rcx
    mov rcx, 0
    call pci_config_read
    cmp eax, edi
    jne .next_device
    
    ; خواندن شناسه دستگاه
    mov rdx, rcx
    mov rcx, 2
    call pci_config_read
    shr eax, 16
    cmp eax, esi
    jne .next_device
    
    ; دستگاه پیدا شد
    mov rax, rbx
    shl rax, 16
    mov ax, cx
    ret
    
.next_device:
    inc rcx
    cmp rcx, 32
    jl .device_loop
    
.next_bus:
    inc rbx
    cmp rbx, 256
    jl .bus_loop
    
    ; دستگاه پیدا نشد
    xor rax, rax
    ret
    