; رجیسترهای کنترل
CR0_PE         equ 1 << 0    ; Protected Mode Enable
CR0_MP         equ 1 << 1    ; Monitor Coprocessor
CR0_EM         equ 1 << 2    ; Emulation
CR0_TS         equ 1 << 3    ; Task Switched
CR0_ET         equ 1 << 4    ; Extension Type
CR0_NE         equ 1 << 5    ; Numeric Error
CR0_WP         equ 1 << 16   ; Write Protect
CR0_AM         equ 1 << 18   ; Alignment Mask
CR0_NW         equ 1 << 29   ; Not Write-through
CR0_CD         equ 1 << 30   ; Cache Disable
CR0_PG         equ 1 << 31   ; Paging

CR4_PSE        equ 1 << 4    ; Page Size Extension
CR4_PAE        equ 1 << 5    ; Physical Address Extension
CR4_MCE        equ 1 << 6    ; Machine Check Enable
CR4_PGE        equ 1 << 7    ; Page Global Enable
CR4_PCE        equ 1 << 8    ; Performance Monitoring Counter Enable
CR4_OSFXSR     equ 1 << 9    ; OS FXSAVE/FXRSTOR Support
CR4_OSXMMEXCPT equ 1 << 10   ; OS Unmasked Exception Support
CR4_UMIP       equ 1 << 11   ; User-Mode Instruction Prevention
CR4_FSGSBASE   equ 1 << 16   ; FSGSBASE Support
CR4_PCIDE      equ 1 << 17   ; PCID Enable
CR4_OSXSAVE    equ 1 << 18   ; XSAVE and Processor Extended States Enable
CR4_SMEP       equ 1 << 20   ; Supervisor Mode Execution Prevention Enable
CR4_SMAP       equ 1 << 21   ; Supervisor Mode Access Prevention Enable

; رجیسترهای مدل خاص (MSR)
IA32_APIC_BASE     equ 0x1B
IA32_FEATURE_CONTROL equ 0x3A
IA32_SYSENTER_CS   equ 0x174
IA32_SYSENTER_ESP  equ 0x175
IA32_SYSENTER_EIP  equ 0x176
IA32_DEBUGCTL      equ 0x1D9
IA32_PAT           equ 0x277
IA32_EFER          equ 0xC0000080
IA32_STAR          equ 0xC0000081
IA32_LSTAR         equ 0xC0000082
IA32_FMASK         equ 0xC0000084
IA32_FS_BASE       equ 0xC0000100
IA32_GS_BASE       equ 0xC0000101
IA32_KERNEL_GSBASE equ 0xC0000102

; وقفه‌ها
INT_DIVIDE_ERROR   equ 0
INT_DEBUG          equ 1
INT_NMI            equ 2
INT_BREAKPOINT     equ 3
INT_OVERFLOW       equ 4
INT_BOUND_RANGE    equ 5
INT_INVALID_OPCODE equ 6
INT_DEVICE_NOT_AVAIL equ 7
INT_DOUBLE_FAULT   equ 8
INT_COPROC_SEG_OVERRUN equ 9
INT_INVALID_TSS    equ 10
INT_SEG_NOT_PRESENT equ 11
INT_STACK_FAULT    equ 12
INT_GPF            equ 13
INT_PAGE_FAULT     equ 14
INT_FPU_ERROR      equ 16
INT_ALIGNMENT_CHECK equ 17
INT_MACHINE_CHECK  equ 18
INT_SIMD_FP_EXCEPT equ 19
INT_VIRT_EXCEPTION equ 20
