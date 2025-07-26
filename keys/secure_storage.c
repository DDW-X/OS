#include <linux/module.h>
#include <linux/fs.h>
#include <linux/crypto.h>
#include <crypto/hash.h>
#include <linux/scatterlist.h>
#include "kernel_config.h"

#define KEY_SIZE 32

// Securely store keys in kernel memory
static u8 secure_keys[KEY_SIZE];

void secure_key_store(const u8 *key, size_t size) {
    if (size > KEY_SIZE) return;
    
    // Encrypt in-place before storage
    kernel_aes_encrypt(key, size, secure_keys, master_key);
    
    // Wipe original memory
    memset((void *)key, 0, size);
}

void secure_key_retrieve(u8 *buffer, size_t size) {
    if (size > KEY_SIZE) return;
    
    // Decrypt keys
    kernel_aes_decrypt(secure_keys, size, buffer, master_key);
}

// Generate secure random key
void generate_secure_key(u8 *key, size_t size) {
    get_random_bytes(key, size);
}

// HMAC-based key verification
int verify_key_hmac(const u8 *key, size_t key_size, 
                    const u8 *hmac, size_t hmac_size) {
    struct crypto_shash *tfm = crypto_alloc_shash("hmac(sha256)", 0, 0);
    if (IS_ERR(tfm)) return PTR_ERR(tfm);
    
    SHASH_DESC_ON_STACK(desc, tfm);
    u8 computed_hmac[SHA256_DIGEST_SIZE];
    int ret;
    
    ret = crypto_shash_setkey(tfm, hmac_key, hmac_key_size);
    if (ret) goto out;
    
    desc->tfm = tfm;
    ret = crypto_shash_digest(desc, key, key_size, computed_hmac);
    if (ret) goto out;
    
    ret = crypto_memneq(hmac, computed_hmac, hmac_size);
    
out:
    crypto_free_shash(tfm);
    return ret;
}
