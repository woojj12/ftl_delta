#ifndef FTL_H
#define FTL_H

#define GTD_SIZE_PER_BANK  ((((NUM_LPAGES + NUM_BANKS - 1)/NUM_BANKS) * sizeof(UINT32) + BYTES_PER_PAGE - 1) / BYTES_PER_PAGE)


/////////////////
// DRAM buffers
/////////////////

#define NUM_RW_BUFFERS    ((DRAM_SIZE - DRAM_BYTES_OTHER) / BYTES_PER_PAGE - 1)
#define NUM_RD_BUFFERS		(((NUM_RW_BUFFERS / 8) + NUM_BANKS - 1) / NUM_BANKS * NUM_BANKS)
#define NUM_WR_BUFFERS		(NUM_RW_BUFFERS - NUM_RD_BUFFERS)
#define NUM_COPY_BUFFERS	NUM_BANKS_MAX
#define NUM_FTL_BUFFERS		NUM_BANKS
#define NUM_HIL_BUFFERS		1
#define NUM_TEMP_BUFFERS	NUM_BANKS
#define NUM_GC_BUFFERS		NUM_BANKS//(GTD_SIZE_PER_BANK)
#define NUM_TRANS_BUFFERS	NUM_BANKS

#define DRAM_BYTES_OTHER	((NUM_TRANS_BUFFERS + NUM_GC_BUFFERS + NUM_COPY_BUFFERS + NUM_FTL_BUFFERS + NUM_HIL_BUFFERS + NUM_TEMP_BUFFERS) * BYTES_PER_PAGE \
+ BAD_BLK_BMP_BYTES + VCOUNT_BYTES + FTL_TEST_BYTES)

#define WR_BUF_PTR(BUF_ID)	(WR_BUF_ADDR + ((UINT32)(BUF_ID)) * BYTES_PER_PAGE)
#define WR_BUF_ID(BUF_PTR)	((((UINT32)BUF_PTR) - WR_BUF_ADDR) / BYTES_PER_PAGE)
#define RD_BUF_PTR(BUF_ID)	(RD_BUF_ADDR + ((UINT32)(BUF_ID)) * BYTES_PER_PAGE)
#define RD_BUF_ID(BUF_PTR)	((((UINT32)BUF_PTR) - RD_BUF_ADDR) / BYTES_PER_PAGE)

#define _COPY_BUF(RBANK)	(COPY_BUF_ADDR + (RBANK) * BYTES_PER_PAGE)
#define COPY_BUF(BANK)		_COPY_BUF(REAL_BANK(BANK))
#define FTL_BUF(BANK)       (FTL_BUF_ADDR + ((BANK) * BYTES_PER_PAGE))

#define GC_BUF(BANK)		(GC_BUF_ADDR + (BANK) * BYTES_PER_PAGE)

#define TEMP_BUF(BANK)		(TEMP_BUF_ADDR + (BANK) * BYTES_PER_PAGE)
#define TRANS_BUF(BANK)		(TRANS_BUF_ADDR + (BANK) * BYTES_PER_PAGE)

///////////////////////////////
// DRAM segmentation
///////////////////////////////

#define RD_BUF_ADDR			DRAM_BASE										// base address of SATA read buffers
#define RD_BUF_BYTES		(NUM_RD_BUFFERS * BYTES_PER_PAGE)

#define WR_BUF_ADDR			(RD_BUF_ADDR + RD_BUF_BYTES)					// base address of SATA write buffers
#define WR_BUF_BYTES		(NUM_WR_BUFFERS * BYTES_PER_PAGE)

#define COPY_BUF_ADDR		(WR_BUF_ADDR + WR_BUF_BYTES)					// base address of flash copy buffers
#define COPY_BUF_BYTES		(NUM_COPY_BUFFERS * BYTES_PER_PAGE)

#define FTL_BUF_ADDR		(COPY_BUF_ADDR + COPY_BUF_BYTES)				// a buffer dedicated to FTL internal purpose
#define FTL_BUF_BYTES		(NUM_FTL_BUFFERS * BYTES_PER_PAGE)

#define HIL_BUF_ADDR		(FTL_BUF_ADDR + FTL_BUF_BYTES)					// a buffer dedicated to HIL internal purpose
#define HIL_BUF_BYTES		(NUM_HIL_BUFFERS * BYTES_PER_PAGE)

#define TEMP_BUF_ADDR		(HIL_BUF_ADDR + HIL_BUF_BYTES)					// general purpose buffer
#define TEMP_BUF_BYTES		(NUM_TEMP_BUFFERS * BYTES_PER_PAGE)

#define GC_BUF_ADDR			(TEMP_BUF_ADDR + TEMP_BUF_BYTES)
#define GC_BUF_BYTES		(NUM_GC_BUFFERS * BYTES_PER_PAGE)

#define TRANS_BUF_ADDR		(GC_BUF_ADDR + GC_BUF_BYTES)
#define TRANS_BUF_BYTES		(NUM_TRANS_BUFFERS * BYTES_PER_PAGE)

#define BAD_BLK_BMP_ADDR	(TRANS_BUF_ADDR + TRANS_BUF_BYTES)				// bitmap of initial bad blocks
#define BAD_BLK_BMP_BYTES	(((NUM_VBLKS / 8) + DRAM_ECC_UNIT - 1) / DRAM_ECC_UNIT * DRAM_ECC_UNIT)

#define VCOUNT_ADDR			(BAD_BLK_BMP_ADDR + BAD_BLK_BMP_BYTES)//(PAGE_MAP_ADDR + PAGE_MAP_BYTES)
#define VCOUNT_BYTES		((NUM_BANKS * VBLKS_PER_BANK * sizeof(UINT16) + BYTES_PER_SECTOR - 1) / BYTES_PER_SECTOR * BYTES_PER_SECTOR)

#define FTL_TEST_ADDR (VCOUNT_ADDR + VCOUNT_BYTES)
#define FTL_TEST_BYTES (4 * 1024 * 1024) // 4MB

// #define BLKS_PER_BANK		VBLKS_PER_BANK


///////////////////////////////
// FTL public functions
///////////////////////////////

void ftl_open(void);
void ftl_read(UINT32 const lba, UINT32 const num_sectors);
void ftl_write(UINT32 const lba, UINT32 const num_sectors);
void ftl_flush(void);
void ftl_isr(void);

#endif //FTL_H
