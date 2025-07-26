section .text

; تنظیم کنترلر وقفه قابل برنامه‌ریزی (PIC)
remap_pic:
    ; ICW1: شروع مقداردهی اولیه
    mov al, ICW1_INIT | ICW1_ICW4
    out PIC1_COMMAND, al
    out PIC2_COMMAND, al
    
    ; ICW2: آدرس بردار وقفه
    mov al, PIC1_OFFSET
    out PIC1_DATA, al
    mov al, PIC2_OFFSET
    out PIC2_DATA, al
    
    ; ICW3: اتصال PICها
    mov al, 0x04    ; PIC1 به IRQ2 متصل است
    out PIC1_DATA, al
    mov al, 0x02    ; PIC2 به IRQ2 متصل است
    out PIC2_DATA, al
    
    ; ICW4: حالت 8086/88
    mov al, ICW4_8086
    out PIC1_DATA, al
    out PIC2_DATA, al
    
    ; پیکربندی ماسک
    mov al, 0x00    ; تمام وقفه‌ها فعال
    out PIC1_DATA, al
    out PIC2_DATA, al
    ret

; نصب هندلر وقفه سفارشی
install_interrupt_handler:
    ; rdi = شماره وقفه
    ; rsi = آدرس هندلر
    
    cli
    mov rax, rsi
    mov [IDT + rdi * 16], ax        ; آدرس پایین
    shr rax, 16
    mov [IDT + rdi * 16 + 6], ax    ; آدرس بالا
    mov word [IDT + rdi * 16 + 2], KERNEL_CS
    mov byte [IDT + rdi * 16 + 4], 0
    mov byte [IDT + rdi * 16 + 5], 0x8E  ; نوع دروازه وقفه
    sti
    ret
    