#!/usr/bin/env python3
import os
import random
import struct
import hashlib
from cryptography.hazmat.primitives.ciphers import Cipher, algorithms, modes
from cryptography.hazmat.backends import default_backend

def generate_dynamic_config():
    """تولید پیکربندی پویا برای سیستم ضد دیباگ"""
    config = {
        'xor_key': random.randint(0, 0xFFFFFFFF),
        'morph_algorithm': random.choice(['xor', 'add', 'rol', 'ror', 'mixed']),
        'check_intervals': random.randint(500, 5000),
        'junk_code_level': random.randint(1, 10),
        'crc_seed': os.urandom(16),
        'obfuscation_depth': random.randint(1, 5),
        'vm_detection_mode': random.choice(['aggressive', 'stealth', 'balanced']),
        'self_healing': random.choice([True, False])
    }
    
    # افزودن امضای امنیتی
    config_hash = hashlib.sha256(str(config).encode()).digest()
    config['signature'] = config_hash
    
    return config

def save_config_to_binary(config):
    """ذخیره پیکربندی در فرمت باینری"""
    with open('anti_debug.cfg', 'wb') as f:
        # نوشتن XOR key
        f.write(struct.pack('I', config['xor_key']))
        
        # نوشتن الگوریتم مبهم‌سازی
        alg_map = {'xor': 0, 'add': 1, 'rol': 2, 'ror': 3, 'mixed': 4}
        f.write(struct.pack('B', alg_map[config['morph_algorithm']]))
        
        # نوشتن فواصل بررسی
        f.write(struct.pack('I', config['check_intervals']))
        
        # نوشتن سطح کد بی‌معنی
        f.write(struct.pack('B', config['junk_code_level']))
        
        # نوشتن seed برای CRC
        f.write(config['crc_seed'])
        
        # نوشتن عمق مبهم‌سازی
        f.write(struct.pack('B', config['obfuscation_depth']))
        
        # نوشتن حالت تشخیص VM
        mode_map = {'aggressive': 0, 'stealth': 1, 'balanced': 2}
        f.write(struct.pack('B', mode_map[config['vm_detection_mode']]))
        
        # نوشتن وضعیت خودترمیمی
        f.write(struct.pack('?', config['self_healing']))
        
        # نوشتن امضا
        f.write(config['signature'])

def verify_config_integrity():
    """بررسی صحت پیکربندی"""
    with open('anti_debug.cfg', 'rb') as f:
        data = f.read()
        
    # استخراج امضا
    signature = data[-32:]
    config_data = data[:-32]
    
    # محاسبه هش
    config_hash = hashlib.sha256(config_data).digest()
    
    # مقایسه با امضای ذخیره شده
    if config_hash != signature:
        print("Config integrity check failed! Potential tampering detected.")
        return False
    
    return True

if __name__ == "__main__":
    # تولید و ذخیره پیکربندی
    config = generate_dynamic_config()
    save_config_to_binary(config)
    
    # تأیید صحت
    if verify_config_integrity():
        print("Config generated and verified successfully.")
    else:
        print("Config verification failed.")

class ConfigGenerator:
    def __init__(self):
        self.config = {
            'xor_key': random.randint(0, 0xFFFFFFFF),
            'check_intervals': random.randint(1000, 10000),
            'junk_code_level': random.randint(1, 10),
            'crc_seed': os.urandom(16),
            'vm_detection_mode': random.choice(['aggressive', 'stealth', 'balanced']),
            'self_healing': random.choice([True, False]),
            'morph_algorithm': random.choice(['xor', 'aes', 'rc4', 'chacha'])
        }
        
        # تولید کلیدهای رمزنگاری
        self.generate_encryption_keys()
        
        # محاسبه امضای دیجیتال
        self.calculate_signature()
    
    def generate_encryption_keys(self):
        """تولید کلیدهای رمزنگاری پویا"""
        self.config['aes_key'] = os.urandom(32)
        self.config['rc4_key'] = os.urandom(16)
        self.config['chacha_key'] = os.urandom(32)
        self.config['chacha_nonce'] = os.urandom(12)
    
    def calculate_signature(self):
        """محاسبه امضای دیجیتال برای پیکربندی"""
        config_str = str(self.config).encode()
        self.config['signature'] = hashlib.sha3_256(config_str).digest()
    
    def encrypt_config(self, output_file):
        """رمزنگاری و ذخیره پیکربندی"""
        # تبدیل به باینری
        config_data = self.serialize_config()
        
        # رمزنگاری با AES
        iv = os.urandom(16)
        cipher = Cipher(algorithms.AES(self.config['aes_key']), modes.CFB(iv), backend=default_backend())
        encryptor = cipher.encryptor()
        encrypted_data = encryptor.update(config_data) + encryptor.finalize()
        
        # ذخیره فایل
        with open(output_file, 'wb') as f:
            f.write(iv)
            f.write(encrypted_data)
    
    def serialize_config(self):
        """سریال‌سازی پیکربندی به باینری"""
        data = b''
        # XOR Key
        data += struct.pack('I', self.config['xor_key'])
        # Check Intervals
        data += struct.pack('I', self.config['check_intervals'])
        # Junk Code Level
        data += struct.pack('B', self.config['junk_code_level'])
        # CRC Seed
        data += self.config['crc_seed']
        # VM Detection Mode
        mode_map = {'aggressive': 0, 'stealth': 1, 'balanced': 2}
        data += struct.pack('B', mode_map[self.config['vm_detection_mode']])
        # Self Healing
        data += struct.pack('?', self.config['self_healing'])
        # Morph Algorithm
        alg_map = {'xor': 0, 'aes': 1, 'rc4': 2, 'chacha': 3}
        data += struct.pack('B', alg_map[self.config['morph_algorithm']])
        # Signature
        data += self.config['signature']
        return data

if __name__ == "__main__":
    print("[*] Generating dynamic anti-debug configuration...")
    generator = ConfigGenerator()
    generator.encrypt_config("advanced_anti_debug.cfg")
    print("[+] Configuration generated and encrypted successfully!")
    