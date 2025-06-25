section .text

; تشخیص هوک‌های سیستمی
detect_kernel_hooks:
    ; بررسی SSDT
    call check_ssdt_integrity

    ; بررسی IDT
    call check_idt_integrity

    ; بررسی IRP Handler
    call check_irp_handlers

    ; بررسی inline hooks
    call detect_inline_hooks
    ret

; بررسی صحت SSDT
check_ssdt_integrity:
    ; دریافت آدرس SSDT
    mov rax, [KeServiceDescriptorTable]

    ; بررسی هر تابع در SSDT
    mov rcx, [rax + SERVICE_TABLE_SIZE]
    mov rsi, [rax + SERVICE_TABLE_BASE]
.check_loop:
    ; بررسی آدرس تابع
    mov rdi, [rsi]
    call validate_kernel_address
    jc .hook_detected

    ; بررسی امضای کد
    call calculate_function_hash
    cmp eax, [expected_hash]
    jne .hook_detected

    add rsi, 8
    loop .check_loop
    ret

.hook_detected:
    call kernel_hook_response
    ret

; پاسخ به تشخیص هوک هسته
kernel_hook_response:
    ; غیرفعال‌کردن دیباگر
    mov dr7, 0

    ; پاک‌کردن رجیسترهای دیباگ
    xor eax, eax
    mov dr0, eax
    mov dr1, eax
    mov dr2, eax
    mov dr3, eax

    ; تخریب داده‌های دیباگ
    call destroy_debug_data
    ret
    