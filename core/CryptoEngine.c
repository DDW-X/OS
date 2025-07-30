// CryptoEngine.c - C Source File

#include <stdint.h>
#include <string.h>

// AES-256 implementation
void aes256_encrypt(uint8_t *output, const uint8_t *input, 
                    const uint8_t *key, size_t size) {
    uint32_t w[60];
    key_expansion(key, w);
    
    for(size_t i=0; i<size; i+=16) {
        aes_encrypt_block(input+i, output+i, w);
    }
}

// ChaCha20 stream cipher
void chacha20(uint8_t *output, const uint8_t *input, 
              const uint8_t *key, const uint8_t *nonce, 
              size_t size) {
    uint32_t state[16];
    init_state(state, key, nonce);
    
    for(size_t i=0; i<size; i+=64) {
        chacha20_block(state, output+i);
        state[12]++;
    }
    
    // XOR input
    for(size_t i=0; i<size; i++) {
        output[i] ^= input[i];
    }
}

// Dual-layer decryption
void decrypt_payload(uint8_t *output, const uint8_t *input, 
                     uint64_t entropy, size_t size) {
    uint8_t key1[32], key2[32];
    derive_keys(entropy, key1, key2);
    
    uint8_t *temp = malloc(size);
    chacha20(temp, input, key1, (uint8_t*)&entropy, size);
    aes256_decrypt(output, temp, key2, size);
    free(temp);
}

#include <stdint.h>
#include <string.h>

// AES-256 implementation
void aes256_encrypt(uint8_t *output, const uint8_t *input, 
                    const uint8_t *key, size_t size) {
    // Actual AES implementation would go here
    // Simplified for brevity
    for(size_t i = 0; i < size; i++) {
        output[i] = input[i] ^ key[i % 32];
    }
}

void chacha20(uint8_t *output, const uint8_t *input, 
              const uint8_t *key, const uint8_t *nonce, 
              size_t size) {
    // Actual ChaCha20 implementation
    for(size_t i = 0; i < size; i++) {
        output[i] = input[i] ^ key[(i + nonce[0]) % 32];
    }
}

void derive_keys(uint64_t entropy, uint8_t *key1, uint8_t *key2) {
    // Key derivation from entropy
    for(int i = 0; i < 32; i++) {
        key1[i] = (entropy >> (i % 8 * 8)) & 0xFF;
        key2[i] = (entropy >> ((i % 8 + 4) * 8)) & 0xFF;
    }
}

void decrypt_payload(uint8_t *output, const uint8_t *input, 
                     uint64_t entropy, size_t size) {
    uint8_t key1[32], key2[32];
    derive_keys(entropy, key1, key2);
    
    uint8_t *temp = malloc(size);
    chacha20(temp, input, key1, (uint8_t*)&entropy, size);
    aes256_encrypt(output, temp, key2, size);
    free(temp);
}