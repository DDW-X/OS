#include <crypto/skcipher.h>
#include <crypto/hash.h>
#include "phantom_defs.h"

// رمزنگاری AES-256
int encrypt_data(char *plaintext, int pt_len, char *key, char *iv, char *ciphertext) {
    struct crypto_skcipher *tfm = crypto_alloc_skcipher("cbc(aes)", 0, 0);
    struct skcipher_request *req;
    struct scatterlist sg_in, sg_out;
    int ret;
    
    if (IS_ERR(tfm)) return PTR_ERR(tfm);
    
    crypto_skcipher_setkey(tfm, key, 32);
    
    req = skcipher_request_alloc(tfm, GFP_KERNEL);
    if (!req) {
        crypto_free_skcipher(tfm);
        return -ENOMEM;
    }
    
    sg_init_one(&sg_in, plaintext, pt_len);
    sg_init_one(&sg_out, ciphertext, pt_len);
    
    skcipher_request_set_crypt(req, &sg_in, &sg_out, pt_len, iv);
    ret = crypto_skcipher_encrypt(req);
    
    skcipher_request_free(req);
    crypto_free_skcipher(tfm);
    return ret;
}

// احراز هویت HMAC-SHA256
int verify_hmac(char *data, int len, char *key, char *hmac) {
    struct crypto_shash *tfm = crypto_alloc_shash("hmac(sha256)", 0, 0);
    SHASH_DESC_ON_STACK(desc, tfm);
    char computed_hmac[32];
    int ret;
    
    if (IS_ERR(tfm)) return PTR_ERR(tfm);
    
    crypto_shash_setkey(tfm, key, strlen(key));
    
    desc->tfm = tfm;
    ret = crypto_shash_digest(desc, data, len, computed_hmac);
    
    crypto_free_shash(tfm);
    return memcmp(computed_hmac, hmac, 32);
}
