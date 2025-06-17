#!/bin/bash
# Generate cryptographic keys for DeepSick

KEY_DIR="keys"
mkdir -p "$KEY_DIR"

# Generate RSA-4096 signing key
openssl genrsa -out "$KEY_DIR/signing.key" 4096
chmod 600 "$KEY_DIR/signing.key"

# Generate self-signed certificate
openssl req -new -x509 -key "$KEY_DIR/signing.key" \
    -out "$KEY_DIR/certificate.pem" -days 365 \
    -subj "/C=XX/ST=Classified/L=Undisclosed/O=DeepSick/OU=CyberOps/CN=deepsick.internal"

# Generate AES-256 encryption key
openssl rand -out "$KEY_DIR/aes.key" 32

# Generate HMAC-SHA256 key
openssl rand -out "$KEY_DIR/hmac.key" 64

# Generate secure entropy seed
openssl rand -out "$KEY_DIR/entropy.seed" 1024

echo "[+] Cryptographic keys generated in $KEY_DIR/"
