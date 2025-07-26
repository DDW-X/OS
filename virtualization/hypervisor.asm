section .text

; فعال‌سازی محافظت مبتنی بر هایپروایزر
enable_hypervisor_protection:
    ; بررسی پشتیبانی از VMX
    mov eax, 1
    cpuid
    test ecx, (1 << 5)  ; VMX bit
    jz .no_hypervisor

    ; ایجاد VM
    call create_secure_vm

    ; انتقال اجرا به VM
    call enter_vmx_operation
    ret

.no_hypervisor:
    ; استفاده از تکنیک‌های جایگزین
    call fallback_anti_debug
    ret

; اجرای کد حیاتی در VM
execute_in_vm:
    ; تنظیم محیط امن
    call setup_secure_environment

    ; رمزگشایی کد
    call decrypt_sensitive_code

    ; اجرای کد
    call sensitive_operation

    ; پاک‌کردن ردپا
    call clean_secure_environment
    ret

; تشخیص محیط‌های مجازی
detect_virtual_environment:
    ; بررسی ابرنشان‌ها
    mov eax, 0x40000000
    cpuid
    cmp eax, 0x40000001
    jb .no_hypervisor
    test ecx, 0x80000000  ; Hypervisor present bit
    jnz .virtual_environment

    ; تکنیک Red Pill
    sidt [idtr]
    mov eax, [idtr + 2]  ; Base address high bits
    cmp eax, 0xFF000000
    ja .virtual_environment

    ; تکنیک VMware backdoor
    mov eax, 0x564D5868  ; 'VMXh'
    xor ebx, ebx
    mov ecx, 10
    mov edx, 0x5658
    in eax, dx
    cmp ebx, 0x564D5868
    je .virtual_environment

.no_hypervisor:
    clc
    ret

.virtual_environment:
    stc
    ret
    