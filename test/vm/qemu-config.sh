// placeholder
#!/bin/bash
# QEMU configuration for testing
qemu-system-x86_64 \
    -hda test/vm/virtual-disk.img \
    -bios /usr/share/ovmf/OVMF.fd \
    -m 4096 \
    -smp 4 \
    -cpu host \
    -enable-kvm \
    -net nic \
    -net user \
    -drive file=build/bootkit/bootkit.bin,format=raw,index=0,media=disk \
    -debugcon file:qemu.log \
    -nographic