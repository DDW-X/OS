section .text

; تشخیص دیباگر با استفاده از API‌های سیستمی
detect_debugger:
    ; Check IsDebuggerPresent
    mov eax, [fs:0x30]      ; PEB
    movzx eax, byte [eax+2] ; BeingDebugged
    test eax, eax
    jnz debugger_detected

    ; Check CheckRemoteDebuggerPresent
    push 0
    push esp
    call [CheckRemoteDebuggerPresent]
    test eax, eax
    jnz debugger_detected

    ; Check NtGlobalFlag
    mov eax, [fs:0x30]      ; PEB
    mov eax, [eax+0x68]     ; NtGlobalFlag
    and eax, 0x70           ; FLG_HEAP_ENABLE_TAIL_CHECK | FLG_HEAP_ENABLE_FREE_CHECK | FLG_HEAP_VALIDATE_PARAMETERS
    test eax, eax
    jnz debugger_detected

    ; Hardware breakpoint detection
    mov eax, [dr0]
    test eax, eax
    jnz debugger_detected
    mov eax, [dr1]
    test eax, eax
    jnz debugger_detected
    mov eax, [dr2]
    test eax, eax
    jnz debugger_detected
    mov eax, [dr3]
    test eax, eax
    jnz debugger_detected

    ret

debugger_detected:
    ; فعال‌سازی اقدامات ضد دیباگ
    call anti_debug_response
    ret

; تشخیص دیباگر با دسترسی مستقیم به ساختارهای سیستمی
detect_deep_debugger:
    ; دسترسی مستقیم به PEB
    mov eax, [fs:0x30]      ; PEB
    cmp byte [eax+2], 0     ; BeingDebugged
    jne debugger_detected

    ; بررسی ProcessDebugPort
    push 0
    push 4
    push esp
    push 0x7             ; ProcessDebugPort
    push -1              ; Current process
    call [NtQueryInformationProcess]
    test eax, eax
    jnz .exit
    pop eax
    test eax, eax
    jnz debugger_detected

    ; بررسی ProcessDebugFlags
    push 0
    push 4
    push esp
    push 0x1F            ; ProcessDebugFlags
    push -1              ; Current process
    call [NtQueryInformationProcess]
    test eax, eax
    jnz .exit
    pop eax
    cmp eax, 0
    je debugger_detected

.exit:
    ret
    