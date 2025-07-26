#!/usr/bin/env python3
# رمزنگاری پیشرفته پیلودها با الگوریتم‌های سفارشی

from cryptography.hazmat.primitives.ciphers import Cipher, algorithms, modes
from cryptography.hazmat.primitives import hashes, hmac
from cryptography.hazmat.primitives.kdf.pbkdf2 import PBKDF2HMAC
from cryptography.hazmat.backends import default_backend
import os
import argparse
import struct

class PayloadEncryptor:
    def __init__(self, key):
        self.key = self.derive_key(key)
        self.iv = os.urandom(16)
    
    def derive_key(self, password):
        """مشتق‌گیری کلید از رمز عبور"""
        salt = b'omni-zero-encryption-salt'
        kdf = PBKDF2HMAC(
            algorithm=hashes.SHA3_512(),
            length=64,
            salt=salt,
            iterations=100000,
            backend=default_backend()
        )
        return kdf.derive(password.encode())
    
    def encrypt(self, data):
        """رمزنگاری داده با الگوریتم ترکیبی"""
        # رمزنگاری با AES
        aes_key = self.key[:32]
        aes_cipher = Cipher(
            algorithms.AES(aes_key),
            modes.CTR(self.iv),
            backend=default_backend()
        )
        encryptor = aes_cipher.encryptor()
        aes_encrypted = encryptor.update(data) + encryptor.finalize()
        
        # رمزنگاری با ChaCha20
        chacha_key = self.key[32:64]
        chacha_cipher = Cipher(
            algorithms.ChaCha20(chacha_key, self.iv),
            mode=None,
            backend=default_backend()
        )
        encryptor = chacha_cipher.encryptor()
        chacha_encrypted = encryptor.update(aes_encrypted)
        
        # محاسبه HMAC
        h = hmac.HMAC(self.key, hashes.SHA3_256(), backend=default_backend())
        h.update(chacha_encrypted)
        hmac_value = h.finalize()
        
        return self.iv + hmac_value + chacha_encrypted
    
    def decrypt(self, data):
        """رمزگشایی داده‌های رمز شده"""
        # استخراج اجزا
        iv = data[:16]
        hmac_value = data[16:48]
        payload = data[48:]
        
        # تأیید HMAC
        h = hmac.HMAC(self.key, hashes.SHA3_256(), backend=default_backend())
        h.update(payload)
        try:
            h.verify(hmac_value)
        except:
            raise ValueError("HMAC verification failed")
        
        # رمزگشایی ChaCha20
        chacha_key = self.key[32:64]
        chacha_cipher = Cipher(
            algorithms.ChaCha20(chacha_key, iv),
            mode=None,
            backend=default_backend()
        )
        decryptor = chacha_cipher.decryptor()
        chacha_decrypted = decryptor.update(payload)
        
        # رمزگشایی AES
        aes_key = self.key[:32]
        aes_cipher = Cipher(
            algorithms.AES(aes_key),
            modes.CTR(iv),
            backend=default_backend()
        )
        decryptor = aes_cipher.decryptor()
        return decryptor.update(chacha_decrypted) + decryptor.finalize()

def main():
    parser = argparse.ArgumentParser(description="Payload Encryption Tool")
    parser.add_argument('--key', required=True, help="Encryption key")
    parser.add_argument('--input', required=True, help="Input file")
    parser.add_argument('--output', required=True, help="Output file")
    args = parser.parse_args()
    
    encryptor = PayloadEncryptor(args.key)
    
    with open(args.input, 'rb') as f:
        data = f.read()
    
    encrypted = encryptor.encrypt(data)
    
    with open(args.output, 'wb') as f:
        f.write(encrypted)

if __name__ == "__main__":
    main()
    