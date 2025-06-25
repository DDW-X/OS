; نقشه حافظه پایه
MEM_BASE_LOW         equ 0x00000000
MEM_BASE_HIGH        equ 0x000A0000
MEM_VIDEO_ROM        equ 0x000C0000
MEM_BIOS_ROM         equ 0x000F0000
MEM_TOP_LOW          equ 0x000FFFFF

; حافظه توسعه یافته
MEM_EXT_BASE         equ 0x00100000
MEM_EXT_END          equ 0x0FFFFFFF

; حافظه بالا
MEM_HIGH_BASE        equ 0x10000000
MEM_HIGH_END         equ 0xFFFFFFFF

; حافظه هسته
KERNEL_BASE          equ 0xFFFF800000000000
KERNEL_STACK_TOP     equ 0xFFFF800000200000
KERNEL_HEAP_BASE     equ 0xFFFF800000300000

; حافظه کاربر
USER_BASE            equ 0x0000000000400000
USER_STACK_TOP       equ 0x00007FFFFFFFFFFF

; MMIO
MMIO_BASE            equ 0xFFFFF00000000000
APIC_BASE            equ MMIO_BASE + 0xFEE00000
HPET_BASE            equ MMIO_BASE + 0xFED00000
PCI_ECAM_BASE        equ MMIO_BASE + 0xE0000000

; حافظه اختصاصی سخت‌افزار
ACPI_TABLES          equ 0x7FF00000
SMBIOS_TABLE         equ 0x000F0000

; رجیسترهای PCI
PCI_CONFIG_ADDRESS   equ 0xCF8
PCI_CONFIG_DATA      equ 0xCFC

; رجیسترهای SPI
SPI_CONTROLLER_BASE  equ 0xFED01000
SPI_HSFS             equ 0x04  ; Hardware Sequencing Flash Status
SPI_HSFC             equ 0x06  ; Hardware Sequencing Flash Control
SPI_FADDR            equ 0x08  ; Flash Address
SPI_FDATA0           equ 0x10  ; Flash Data 0-3
SPI_FDATA1           equ 0x14
SPI_FDATA2           equ 0x18
SPI_FDATA3           equ 0x1C
SPI_FPR0             equ 0x84  ; Flash Protected Range 0

; رجیسترهای NVMe
NVME_CONTROLLER_BASE equ 0xFEB80000
NVME_CAP             equ 0x00  ; Capabilities
NVME_VS              equ 0x08  ; Version
NVME_INTMS           equ 0x0C  ; Interrupt Mask Set
NVME_INTMC           equ 0x10  ; Interrupt Mask Clear
NVME_CC              equ 0x14  ; Configuration
NVME_CSTS            equ 0x1C  ; Status
NVME_SQ0TDBL         equ 0x1000  ; Submission Queue 0 Tail Doorbell
NVME_CQ0HDBL         equ 0x1004  ; Completion Queue 0 Head Doorbell
NVME_PRPTR           equ 0x2000  ; PRP Entry
