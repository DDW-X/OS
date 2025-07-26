section .text
global load_pe_module, load_elf_module

%include "pe_elf.inc"

load_pe_module:
    ; پارس هدر PE
    mov rsi, [rdi + PE_HEADER_OFFSET]
    add rsi, rdi                  ; آدرس هدر PE

    ; بارگذاری سکشن‌ها
    movzx rcx, word [rsi + PE_NUMBER_OF_SECTIONS]
    mov rbx, rsi
    add rbx, PE_SECTION_HEADERS_OFFSET

.load_section:
    mov r8, [rbx + SECTION_VIRT_ADDR]
    mov r9, [rbx + SECTION_RAW_SIZE]
    mov r10, [rbx + SECTION_RAW_OFFSET]
    add r10, rdi                  ; آدرس داده خام

    ; کپی داده به حافظه مجازی
    mov rdi, r8
    mov rsi, r10
    mov rdx, r9
    call memcpy

    ; تنظیم مجوزهای حافظه
    mov rdi, r8
    mov rsi, r9
    mov dl, [rbx + SECTION_CHARACTERISTICS]
    call set_section_permissions

    add rbx, SECTION_HEADER_SIZE
    loop .load_section

    ; تنظیم نقطه ورود
    mov rax, [rsi + PE_ENTRY_POINT]
    ret

load_elf_module:
    ; پارس هدر ELF
    mov rsi, rdi

    ; بارگذاری سگمنت‌ها
    mov rcx, [rsi + ELF_PH_NUM]
    mov rbx, rsi
    add rbx, [rsi + ELF_PH_OFF]

.load_segment:
    cmp [rbx + PH_TYPE], PT_LOAD
    jne .next_segment

    mov r8, [rbx + PH_VADDR]      ; آدرس مجازی
    mov r9, [rbx + PH_FILESZ]     ; اندازه فایل
    mov r10, [rbx + PH_OFFSET]    ; آفست فایل
    add r10, rdi                  ; آدرس داده خام

    ; کپی داده
    mov rdi, r8
    mov rsi, r10
    mov rdx, r9
    call memcpy

    ; تنظیم مجوزها
    mov rdi, r8
    mov rsi, [rbx + PH_MEMSZ]
    mov dl, [rbx + PH_FLAGS]
    call set_segment_permissions

.next_segment:
    add rbx, ELF_PH_SIZE
    loop .load_segment

    ; تنظیم نقطه ورود
    mov rax, [rsi + ELF_ENTRY]
    ret

set_section_permissions:
    ; تبدیل مجوزهای PE به سیستم
    test dl, IMAGE_SCN_MEM_EXECUTE
    jnz .exec
    test dl, IMAGE_SCN_MEM_READ
    jnz .read
    ret

.exec:
    ; تنظیم حافظه اجرایی
    push rdi
    push rsi
    call set_memory_executable
    pop rsi
    pop rdi
    ret
    