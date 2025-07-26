section .text

; فراخوانی وقفه BIOS
bios_interrupt:
    ; rdi = شماره وقفه
    ; rsi = ساختور رجیسترها
    
    ; ذخیره وضعیت 64 بیتی
    pushaq
    
    ; تغییر به حالت 16 بیتی واقعی
    jmp 0x20:protected_to_real

protected_to_real:
    bits 32
    mov ax, 0x28
    mov ds, ax
    mov es, ax
    mov fs, ax
    mov gs, ax
    mov ss, ax
    
    ; پرش به کد 16 بیتی
    jmp 0x30:real_mode_entry

real_mode_entry:
    bits 16
    ; غیرفعال‌کردن پیجینگ
    mov eax, cr0
    and eax, ~CR0_PG
    mov cr0, eax
    
    ; غیرفعال‌کردن حالت حفاظت شده
    mov eax, cr0
    and eax, ~CR0_PE
    mov cr0, eax
    
    ; پرش به کد اصلی
    jmp 0:real_mode_main

real_mode_main:
    ; تنظیم بخش‌ها
    xor ax, ax
    mov ds, ax
    mov es, ax
    mov fs, ax
    mov gs, ax
    mov ss, ax
    
    ; بازیابی رجیسترها
    mov eax, [rsi + 0]
    mov ebx, [rsi + 4]
    mov ecx, [rsi + 8]
    mov edx, [rsi + 12]
    mov ebp, [rsi + 16]
    mov edi, [rsi + 20]
    mov esi, [rsi + 24]
    
    ; فراخوانی وقفه
    int di
    
    ; ذخیره نتیجه
    push eax
    push ebx
    push ecx
    push edx
    push ebp
    push edi
    push esi
    
    ; بازگشت به حالت حفاظت شده
    mov eax, cr0
    or eax, CR0_PE
    mov cr0, eax
    
    ; پرش به کد 32 بیتی
    jmp 0x20:real_to_protected

real_to_protected:
    bits 32
    ; فعال‌کردن پیجینگ
    mov eax, cr0
    or eax, CR0_PG
    mov cr0, eax
    
    ; تنظیم بخش‌های 64 بیتی
    mov ax, 0x10
    mov ds, ax
    mov es, ax
    mov fs, ax
    mov gs, ax
    mov ss, ax
    
    ; پرش به کد 64 بیتی
    jmp 0x40:protected_to_long

protected_to_long:
    bits 64
    ; بازیابی وضعیت
    popaq
    
    ; بازگشت
    ret
