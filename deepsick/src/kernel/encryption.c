// placeholder
#include <linux/module.h>
#include <linux/crypto.h>
#include <crypto/internal/skcipher.h>
#include <crypto/aes.h>
#include "kernel_config.h"

#define AES_KEY_SIZE 32
#define AES_BLOCK_SIZE 16

// Kernel-space AES-256 encryption
int kernel_aes_encrypt(const u8 *plaintext, size_t size, u8 *ciphertext, const u8 *key) {
    struct crypto_skcipher *tfm = NULL;
    struct skcipher_request *req = NULL;
    struct scatterlist sg_in, sg_out;
    DECLARE_CRYPTO_WAIT(wait);
    int ret = 0;
    u8 iv[AES_BLOCK_SIZE] = {0};

    // Allocate transform
    tfm = crypto_alloc_skcipher("cbc(aes)", 0, 0);
    if (IS_ERR(tfm)) {
        ret = PTR_ERR(tfm);
        goto out;
    }

    // Set key
    ret = crypto_skcipher_setkey(tfm, key, AES_KEY_SIZE);
    if (ret) {
        goto free_tfm;
    }

    // Allocate request
    req = skcipher_request_alloc(tfm, GFP_KERNEL);
    if (!req) {
        ret = -ENOMEM;
        goto free_tfm;
    }

    // Set up scatterlists
    sg_init_one(&sg_in, plaintext, size);
    sg_init_one(&sg_out, ciphertext, size);
    skcipher_request_set_callback(req, CRYPTO_TFM_REQ_MAY_BACKLOG | CRYPTO_TFM_REQ_MAY_SLEEP,
                                  crypto_req_done, &wait);
    skcipher_request_set_crypt(req, &sg_in, &sg_out, size, iv);

    // Perform encryption
    ret = crypto_wait_req(crypto_skcipher_encrypt(req), &wait);

free_req:
    skcipher_request_free(req);
free_tfm:
    crypto_free_skcipher(tfm);
out:
    return ret;
}

// Kernel-space AES-256 decryption
int kernel_aes_decrypt(const u8 *ciphertext, size_t size, u8 *plaintext, const u8 *key) {
    struct crypto_skcipher *tfm = NULL;
    struct skcipher_request *req = NULL;
    struct scatterlist sg_in, sg_out;
    DECLARE_CRYPTO_WAIT(wait);
    int ret = 0;
    u8 iv[AES_BLOCK_SIZE] = {0};

    tfm = crypto_alloc_skcipher("cbc(aes)", 0, 0);
    if (IS_ERR(tfm)) {
        ret = PTR_ERR(tfm);
        goto out;
    }

    ret = crypto_skcipher_setkey(tfm, key, AES_KEY_SIZE);
    if (ret) {
        goto free_tfm;
    }

    req = skcipher_request_alloc(tfm, GFP_KERNEL);
    if (!req) {
        ret = -ENOMEM;
        goto free_tfm;
    }

    sg_init_one(&sg_in, ciphertext, size);
    sg_init_one(&sg_out, plaintext, size);
    skcipher_request_set_callback(req, CRYPTO_TFM_REQ_MAY_BACKLOG | CRYPTO_TFM_REQ_MAY_SLEEP,
                                  crypto_req_done, &wait);
    skcipher_request_set_crypt(req, &sg_in, &sg_out, size, iv);

    ret = crypto_wait_req(crypto_skcipher_decrypt(req), &wait);

free_req:
    skcipher_request_free(req);
free_tfm:
    crypto_free_skcipher(tfm);
out:
    return ret;
}
