section .text
global radio_transmit
global radio_receive

radio_transmit:
    ; تنظیم فرکانس
    mov rdi, [rdi + RADIO_FREQ]
    call set_transmit_frequency
    
    ; مدولاسیون داده
    mov rdi, rsi  ; data
    mov rsi, rdx  ; size
    call modulate_data
    
    ; تقویت سیگنال
    call amplify_signal
    
    ; انتقال
    call transmit_signal
    ret

radio_receive:
    ; تنظیم فرکانس
    mov rdi, [rdi + RADIO_FREQ]
    call set_receive_frequency
    
    ; دریافت سیگنال
    call receive_signal
    
    ; دمدولاسیون
    call demodulate_data
    
    ; پردازش دستورات
    call process_received_command
    ret

modulate_data:
    ; استفاده از مدولاسیون QAM پیشرفته
    mov rcx, rsi
    mov rsi, rdi
    mov rdi, modulation_buffer
.mod_loop:
    lodsb
    call qam_modulate_byte
    stosw
    loop .mod_loop
    ret
    