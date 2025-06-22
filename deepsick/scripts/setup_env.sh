// placeholder
#!/bin/bash
# Setup development environment
sudo apt update -y
sudo apt install -y \
    build-essential \
    linux-headers-$(uname -r) \
    nasm \
    gcc-multilib \
    gdb \
    qemu-system-x86 \
    libssl-dev \
    pkg-config \
    sbsigntool \
    efibootmgr \
    git \
    curl

# Generate signing keys
mkdir -p keys
openssl genrsa -out keys/signing.key 4096
openssl req -new -x509 -key keys/signing.key -out keys/certificate.pem -days 365 -subj "/CN=DeepSick Security"

# Create test virtual disk
mkdir -p test/vm
qemu-img create -f qcow2 test/vm/virtual-disk.img 20G

echo "[+] Environment setup complete"