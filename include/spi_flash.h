#ifndef SPI_FLASH_H
#define SPI_FLASH_H

#include <linux/types.h>

// دستورات SPI Flash
#define SPI_CMD_WREN     0x06
#define SPI_CMD_CHIP_ERASE 0xC7
#define SPI_CMD_PAGE_PROG 0x02

// ثوابت زمان‌بندی
#define SPI_TIMEOUT_MS   3000

// توابع
void spi_flash_write_enable(void __iomem *base);
void spi_flash_chip_erase(void __iomem *base);
void spi_flash_page_program(void __iomem *base, u32 addr, u32 data);

#endif // SPI_FLASH_H
