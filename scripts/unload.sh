#!/bin/bash
# اسکریپت حذف حرفه‌ای روت‌کیت سطح کرنل (نسخه صنعتی)

# --- تنظیمات پیشرفته ---
ROOTKIT_SIGNATURE="DSK_ROOTKIT_SIGNATURE"
HIDDEN_PREFIX=".rootkit"
MAGIC_PORT=31337
SERVICE_NAME="rootkit.service"
CRON_JOB="rootkit"

# --- تأیید هویت و احراز صلاحیت ---
if [ "$(id -u)" -ne 0 ]; then
    echo "[-] خطا: این اسکریپت نیاز به دسترسی root دارد!" >&2
    exit 1
fi

# --- توابع پیشرفته ---
log() {
    echo -e "\033[1;34m[*]\033[0m $1"
}

error() {
    echo -e "\033[1;31m[-]\033[0m $1" >&2
}

success() {
    echo -e "\033[1;32m[+]\033[0m $1"
}

# --- تشخیص و حذف ماژول‌های مخفی ---
find_hidden_modules() {
    # تحلیل حافظه کرنل برای یافتن ماژول‌های پنهان
    local hidden_mods=()
    local module_list=$(grep "modules" /proc/kallsyms | awk '{print $4}')
    
    for mod in $module_list; do
        if strings "/sys/module/$mod/".* 2>/dev/null | grep -q "$ROOTKIT_SIGNATURE"; then
            hidden_mods+=("$mod")
        fi
    done
    
    echo "${hidden_mods[@]}"
}

# --- بازگردانی sys_call_table ---
restore_syscall_table() {
    local syscall_table_addr=$(grep 'sys_call_table' /boot/System.map-$(uname -r) | awk '{print $1}')
    if [ -z "$syscall_table_addr" ]; then
        error "پیدا کردن sys_call_table ناموفق بود"
        return 1
    fi

    # ایجاد ماژول بازگردانی دینامیک
    cat > /tmp/syscall_restore.c <<EOF
#include <linux/module.h>
#include <linux/kernel.h>

static unsigned long *syscall_table;

static int __init restore_init(void) {
    syscall_table = (unsigned long *)0x$syscall_table_addr;
    
    // بازگردانی sys_kill
    syscall_table[__NR_kill] = (unsigned long)0x$(grep 'sys_kill' /boot/System.map-$(uname -r) | awk '{print $1}');
    
    // بازگردانی sys_open
    syscall_table[__NR_open] = (unsigned long)0x$(grep 'sys_open' /boot/System.map-$(uname -r) | awk '{print $1}');
    
    // بازگردانی getdents64
    syscall_table[__NR_getdents64] = (unsigned long)0x$(grep 'sys_getdents64' /boot/System.map-$(uname -r) | awk '{print $1}');
    
    return 0;
}

static void __exit restore_exit(void) {
    printk(KERN_INFO "Syscalls restored successfully\\n");
}

module_init(restore_init);
module_exit(restore_exit);
MODULE_LICENSE("GPL");
EOF

    # کامپایل و بارگذاری ماژول
    make -C /lib/modules/$(uname -r)/build M=/tmp modules >/dev/null 2>&1
    insmod /tmp/syscall_restore.ko
    rmmod syscall_restore
    rm -rf /tmp/syscall_restore*
}

# --- غیرفعال‌سازی مکانیزم‌های مقاومت ---
disable_persistence() {
    log "غیرفعال‌سازی مکانیزم‌های مقاومت"
    
    # حذف سرویس سیستمی
    if systemctl is-active --quiet "$SERVICE_NAME"; then
        systemctl stop "$SERVICE_NAME"
        systemctl disable "$SERVICE_NAME"
    fi
    rm -f "/etc/systemd/system/$SERVICE_NAME"
    systemctl daemon-reload
    
    # حذف کرون جاب
    rm -f "/etc/cron.d/$CRON_JOB"
    rm -f "/etc/cron.daily/$CRON_JOB"
    rm -f "/etc/cron.hourly/$CRON_JOB"
    
    # حذف فایل‌های پیکربندی
    find /etc -maxdepth 1 -type f -name "*$CRON_JOB*" -exec rm -f {} \;
    
    # کشتن فرآیندهای مقاوم
    pkill -f "rootkitd"
    pkill -f "\[kworker/0:0H\]"
}

# --- حذف ماژول‌های کرنل ---
unload_modules() {
    log "شروع فرآیند حذف ماژول‌ها"
    
    # لیست ماژول‌های استاندارد
    local std_modules=("stealth" "backdoor" "hooking" "persistence")
    
    for mod in "${std_modules[@]}"; do
        if lsmod | grep -q "^$mod"; then
            log "حذف ماژول: $mod"
            rmmod "$mod" 2>/dev/null || \
            modprobe -r -f "$mod" 2>/dev/null
        fi
    done
    
    # تشخیص و حذف ماژول‌های پنهان
    local hidden_mods=($(find_hidden_modules))
    for mod in "${hidden_mods[@]}"; do
        log "حذف ماژول مخفی: $mod"
        echo 1 > "/sys/module/$mod/parameters/unload" 2>/dev/null || \
        rmmod --force "$mod" 2>/dev/null
    done
    
    # حذف ماژول‌های آلوده شده
    find /lib/modules/$(uname -r) -type f -name "*.ko*" -print0 | while IFS= read -r -d $'\0' module; do
        if strings "$module" | grep -q "$ROOTKIT_SIGNATURE"; then
            local mod_name=$(basename "$module" .ko)
            log "حذف ماژول آلوده: $mod_name"
            rmmod --force "$mod_name" 2>/dev/null
            rm -f "$module"
        fi
    done
}

# --- پاک‌سازی فایل‌های سیستمی ---
clean_system() {
    log "پاک‌سازی فایل‌های سیستمی"
    
    # حذف فایل‌های اجرایی
    rm -f /usr/sbin/rootkitd
    rm -f /usr/bin/.rootkit_util
    rm -f /sbin/.rk_helper
    
    # حذف فایل‌های مخفی
    find / -type f -name "*$HIDDEN_PREFIX*" -exec rm -f {} \; 2>/dev/null
    find / -type f -name "*.rootkit" -exec rm -f {} \; 2>/dev/null
    
    # حذف فایل‌های موقت
    rm -f /tmp/.rk_*
    rm -f /dev/shm/.rk_*
    
    # حذف لاگ‌های مرتبط
    journalctl --vacuum-time=1s
    rm -f /var/log/sysrootkit.log
    rm -f /var/log/*.rk
}

# --- پاک‌سازی حافظه و کش‌ها ---
clean_memory() {
    log "پاک‌سازی حافظه و کش‌ها"
    
    # پاک‌سازی slab cache
    echo 2 > /proc/sys/vm/drop_caches
    
    # پاک‌سازی حافظه اشتراکی
    ipcrm -a 2>/dev/null
    
    # پاک‌سازی حافظه اشتراکی روت‌کیت
    for shm_id in $(ipcs -m | grep "$USER" | awk '{print $2}'); do
        ipcrm -m "$shm_id" 2>/dev/null
    done
}

# --- بستن درگاه‌های مخفی ---
close_hidden_ports() {
    log "بستن درگاه‌های مخفی"
    
    # بستن درگاه جادویی
    local port_pid=$(lsof -i :$MAGIC_PORT | awk 'NR==2 {print $2}')
    if [ -n "$port_pid" ]; then
        kill -9 "$port_pid"
    fi
    
    # بستن تمام درگاه‌های مشکوک
    netstat -tulnp | grep -E '31337|31338|31339' | awk '{print $7}' | cut -d'/' -f1 | xargs kill -9 2>/dev/null
}

# --- بازگردانی تنظیمات سیستم ---
restore_system() {
    log "بازگردانی تنظیمات سیستم"
    
    # بازگردانی مجوزهای فایل
    chmod 644 /etc/passwd
    chmod 600 /etc/shadow
    chmod 644 /etc/group
    
    # بازگردانی SELinux/AppArmor
    if command -v setenforce >/dev/null; then
        setenforce 1
    fi
    if command -v aa-enforce >/dev/null; then
        aa-enforce /etc/apparmor.d/*
    fi
    
    # بازگردانی iptables
    iptables -F
    ip6tables -F
    iptables -X
    ip6tables -X
    iptables -P INPUT ACCEPT
    iptables -P OUTPUT ACCEPT
    iptables -P FORWARD ACCEPT
}

# --- حذف ردپا و بازگردانی نهایی ---
final_cleanup() {
    log "حذف نهایی ردپاها"
    
    # بازگردانی توابع LKM
    depmod -a
    update-initramfs -u
    
    # پاک‌سازی تاریخچه
    history -c
    echo "" > ~/.bash_history
    
    # پاک‌سازی لاگ‌های کرنل
    dmesg -c > /dev/null
    echo "" > /var/log/kern.log
    
    # حذف فایل‌های موقت اسکریپت
    rm -f /tmp/unload_*
    rm -f /tmp/syscall_*
}

# --- تابع اصلی ---
main() {
    # غیرفعال‌سازی مقاومت
    disable_persistence
    
    # بستن درگاه‌های مخفی
    close_hidden_ports
    
    # حذف ماژول‌ها
    unload_modules
    
    # بازگردانی توابع سیستمی
    restore_syscall_table
    
    # پاک‌سازی سیستم
    clean_system
    clean_memory
    
    # بازگردانی تنظیمات
    restore_system
    
    # حذف نهایی ردپاها
    final_cleanup
    
    success "روت‌کیت با موفقیت حذف شد!"
    log "توصیه: سیستم را ریبوت کنید تا تغییرات نهایی اعمال شود"
    log "دستور: sudo reboot -f"
}

# اجرای تابع اصلی
main
