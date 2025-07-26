// placeholder
#!/bin/bash
# Sign all binaries
for bin in build/kernel/*.ko build/user/*; do
    if [ -f "$bin" ]; then
        sbsign --key keys/signing.key \
               --cert keys/certificate.pem \
               --output "${bin}.signed" "$bin"
        echo "Signed: $bin"
    fi
done