section .text

; تشخیص دیباگر با زمان‌سنج با دقت بالا
high_precision_timing_check:
    ; زمان‌سنج با TSC
    rdtsc
    mov [start_tsc], eax
    mov [start_tsc+4], edx

    ; اجرای عملیات تست
    call test_operation

    ; خواندن TSC مجدد
    rdtsc
    sub eax, [start_tsc]
    sbb edx, [start_tsc+4]

    ; بررسی آستانه زمانی
    cmp edx, 0
    ja debugger_detected
    cmp eax, 5000
    ja debugger_detected
    ret

test_operation:
    ; عملیاتی که زمان اجرای آن اندازه‌گیری می‌شود
    mov ecx, 1000
.loop:
    rdrand eax
    bswap eax
    loop .loop
    ret

; تشخیص وقفه‌های دیباگر
debug_interrupt_detection:
    ; ذخیره IDT اصلی
    sidt [idtr_save]

    ; ایجاد IDT جدید
    call setup_custom_idt

    ; تنظیم IDT
    lidt [idtr_custom]

    ; فعال‌سازی وقفه تست
    int 0x41

    ; بازگردانی IDT اصلی
    lidt [idtr_save]

    ; بررسی اجرا شدن هندلر
    cmp [interrupt_handled], 1
    jne debugger_detected
    ret
    