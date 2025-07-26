[BITS 64]
section .text
global omni_access_init

omni_access_init:
    call enable_ring0
    call map_physical_memory
    call install_idt_handler
    call enter_smm
    call dma_transfer
    ; ... سایر فراخوانی‌ها
    ret