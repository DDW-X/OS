#!/usr/bin/env python3
# تولید سیگنال‌های رادیویی مخفی

import numpy as np
import sounddevice as sd
import struct
import time
from scipy.signal import chirp

# تنظیمات پیشرفته سیگنال
SAMPLE_RATE = 192000  # Hz
CARRIER_FREQ = 24000   # Hz
MODULATION_RATE = 1000 # Hz
AMPLITUDE = 0.8

def generate_covert_signal(data):
    """تولید سیگنال حامل داده‌های مخفی"""
    t = np.linspace(0, len(data)/MODULATION_RATE, len(data)*SAMPLE_RATE//MODULATION_RATE)
    carrier = AMPLITUDE * np.sin(2 * np.pi * CARRIER_FREQ * t)
    
    # مدولاسیون دامنه
    modulated = np.zeros_like(t)
    for i, byte in enumerate(data):
        start_idx = i * len(t) // len(data)
        end_idx = (i+1) * len(t) // len(data)
        bit = 1 if byte > 0 else 0
        modulated[start_idx:end_idx] = bit * carrier[start_idx:end_idx]
    
    # افزودن نویز برای استتار
    noise = 0.1 * np.random.normal(size=len(t))
    return modulated + noise

def transmit_radio_signal(data, repeat=3):
    """ارسال داده از طریق امواج رادیویی"""
    signal = generate_covert_signal(data)
    for _ in range(repeat):
        sd.play(signal, SAMPLE_RATE)
        sd.wait()
        time.sleep(0.1)  # وقفه کوتاه بین ارسال‌ها

def receive_radio_signal(duration=5):
    """دریافت و رمزگشایی سیگنال‌های رادیویی"""
    print("[*] Listening for covert signals...")
    recording = sd.rec(int(duration * SAMPLE_RATE), 
                      samplerate=SAMPLE_RATE, 
                      channels=1, 
                      blocking=True)
    
    # پردازش سیگنال
    fft_result = np.fft.rfft(recording[:,0])
    freqs = np.fft.rfftfreq(len(recording), 1/SAMPLE_RATE)
    
    # تشخیص حامل
    carrier_idx = np.argmax(np.abs(fft_result))
    detected_freq = freqs[carrier_idx]
    
    # استخراج داده
    extracted_data = []
    samples_per_bit = SAMPLE_RATE // MODULATION_RATE
    for i in range(0, len(recording), samples_per_bit):
        chunk = recording[i:i+samples_per_bit, 0]
        avg_amplitude = np.mean(np.abs(chunk))
        bit = 1 if avg_amplitude > AMPLITUDE/2 else 0
        extracted_data.append(bit)
    
    return bytes(extracted_data)

# مثال استفاده:
if __name__ == "__main__":
    # ارسال دستور مخفی
    secret_command = b"\xDE\xAD\xBE\xEF"  # دستور نمونه
    transmit_radio_signal(secret_command)
    
    # دریافت پاسخ (در محیط واقعی)
    # response = receive_radio_signal(10)
    # print(f"Received response: {response.hex()}")
    