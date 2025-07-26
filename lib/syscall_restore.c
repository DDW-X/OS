#include <linux/module.h>
#include <linux/kernel.h>
#include <linux/syscalls.h>
#include <linux/kallsyms.h>

static unsigned long *syscall_table;

extern asmlinkage long __x64_sys_kill(pid_t pid, int sig);
extern asmlinkage long __x64_sys_open(const char __user *filename, int flags, umode_t mode);
extern asmlinkage long __x64_sys_getdents64(unsigned int fd, struct linux_dirent64 __user *dirent, unsigned int count);

static int __init restore_init(void) {
    syscall_table = (unsigned long *)kallsyms_lookup_name("sys_call_table");
    
    if (!syscall_table) {
        printk(KERN_ALERT "Syscall table not found!\n");
        return -EINVAL;
    }

    // بازگردانی sys_kill
    syscall_table[__NR_kill] = (unsigned long)__x64_sys_kill;
    
    // بازگردانی sys_open
    syscall_table[__NR_open] = (unsigned long)__x64_sys_open;
    
    // بازگردانی getdents64
    syscall_table[__NR_getdents64] = (unsigned long)__x64_sys_getdents64;
    
    printk(KERN_INFO "Rootkit syscalls restored successfully\n");
    return 0;
}

static void __exit restore_exit(void) {
    printk(KERN_INFO "Syscall restore module unloaded\n");
}

module_init(restore_init);
module_exit(restore_exit);
MODULE_LICENSE("GPL");
MODULE_AUTHOR("DeepSeek R1");
MODULE_DESCRIPTION("Kernel Syscall Restoration Module");
