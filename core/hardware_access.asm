section .text

; دسترسی به پورت‌های I/O
inb:
    ; rdi = پورت
    mov rdx, rdi
    in al, dx
    ret

outb:
    ; rdi = پورت
    ; rsi = داده
    mov rdx, rdi
    mov rax, rsi
    out dx, al
    ret

; دسترسی مستقیم به حافظه ویدیویی
write_to_video_memory:
    ; rdi = آفست
    ; rsi = داده
    mov rax, VGA_BASE
    add rax, rdi
    mov [rax], rsi
    ret

; خواندن/نوشتن CMOS
read_cmos:
    ; rdi = آدرس
    mov al, dil
    out CMOS_INDEX, al
    in al, CMOS_DATA
    ret

write_cmos:
    ; rdi = آدرس
    ; rsi = داده
    mov al, dil
    out CMOS_INDEX, al
    mov al, sil
    out CMOS_DATA, al
    ret
    