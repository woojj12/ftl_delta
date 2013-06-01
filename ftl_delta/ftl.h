// Copyright 2011 INDILINX Co., Ltd.
//
// This file is part of Jasmine.
//
// Jasmine is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// Jasmine is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with Jasmine. See the file COPYING.
// If not, see <http://www.gnu.org/licenses/>.
//
// FASTer FTL header file
//
// Author; Sang-Phil Lim (SKKU VLDB Lab.)
//
// Reference;
//   - Sang-Phil Lim, Sang-Won Lee and Bongki Moon, "FASTer FTL for Enterprise-Class Flash Memory SSDs"
//     IEEE SNAPI 2010: 6th IEEE International Workshop on Storage Network Architecture and Parallel I/Os, May 2010
//

#ifndef FTL_H
#define FTL_H

//JJ
#include "delta.h"

/////////////////
// DRAM buffers
/////////////////

#define NUM_RW_BUFFERS                                                          ((DRAM_SIZE - DRAM_BYTES_OTHER) / BYTES_PER_PAGE - 1)
#define NUM_RD_BUFFERS                             	(((NUM_RW_BUFFERS / 8) + NUM_BANKS - 1) / NUM_BANKS * NUM_BANKS)
#define NUM_WR_BUFFERS		(NUM_RW_BUFFERS - NUM_RD_BUFFERS)
#define NUM_COPY_BUFFERS	NUM_BANKS_MAX
#define NUM_FTL_BUFFERS		NUM_BANKS
#define NUM_HIL_BUFFERS		1
#define NUM_TEMP_BUFFERS	3
#define NUM_DELTA_BUFFERS	1
#define NUM_LPA_BUFFERS		NUM_BANKS
#define NUM_GC_BUFFERS		2

#define DRAM_BYTES_OTHER	((NUM_GC_BUFFERS + NUM_LPA_BUFERS + NUM_COPY_BUFFERS + NUM_FTL_BUFFERS + NUM_HIL_BUFFERS + NUM_TEMP_BUFFERS + NUM_DELTA_BUFFERS) * BYTES_PER_PAGE + BAD_BLK_BMP_BYTES \
                             + FTL_BMT_BYTES)

#define WR_BUF_PTR(BUF_ID)	(WR_BUF_ADDR + ((UINT32)(BUF_ID)) * BYTES_PER_PAGE)
#define WR_BUF_ID(BUF_PTR)	((((UINT32)BUF_PTR) - WR_BUF_ADDR) / BYTES_PER_PAGE)
#define RD_BUF_PTR(BUF_ID)	(RD_BUF_ADDR + ((UINT32)(BUF_ID)) * BYTES_PER_PAGE)
#define RD_BUF_ID(BUF_PTR)	((((UINT32)BUF_PTR) - RD_BUF_ADDR) / BYTES_PER_PAGE)
#define TEMP_BUF_PTR(BUF_ID)	(TEMP_BUF_ADDR + ((UINT32)(BUF_ID)) * BYTES_PER_PAGE)
#define TEMP_BUF_ID(BUF_PTR)	((((UINT32)BUF_PTR) - TEMP_BUF_ADDR) / BYTES_PER_PAGE)
#define GC_BUF_PTR(BUF_ID)	(GC_BUF_ADDR + ((UINT32)(BUF_ID)) * BYTES_PER_PAGE)
#define GC_BUF_ID(BUF_PTR)	((((UINT32)BUF_PTR) - GC_BUF_ADDR) / BYTES_PER_PAGE)

#define _COPY_BUF(RBANK)	(COPY_BUF_ADDR + (RBANK) * BYTES_PER_PAGE)
#define COPY_BUF(BANK)		_COPY_BUF(REAL_BANK(BANK))
#define FTL_BUF(BANK)       (FTL_BUF_ADDR + ((BANK) * BYTES_PER_PAGE))

#define DELTA_BUF(BANK)		(DELTA_BUF_ADDR + ((BANK) * BYTES_PER_PAGE))

#define LPA_BUF(BANK)		(LPA_BUF_ADDR + ((BANK) * BYTES_PER_PAGE))

///////////////////////////////
// DRAM segmentation
///////////////////////////////

#define RD_BUF_ADDR		DRAM_BASE										// base address of SATA read buffers
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

#define DELTA_BUF_ADDR		(TEMP_BUF_ADDR + TEMP_BUF_BYTES)
#define DELTA_BUF_BYTES		(NUM_DELTA_BUFFERS * BYTES_PER_PAGE)

#define LPA_BUF_ADDR		(DELTA_BUF_ADDR + DELTA_BUF_BYTES)
#define LPA_BUF_BYTES		(NUM_LPA_BUFFERS * BYTES_PER_PAGE)

#define GC_BUF_ADDR		(LPA_BUF_ADDR + LPA_BUF_BYTES)
#define GC_BUF_BYTES		(NUM_GC_BUFFERS * BYTES_PER_PAGE)

#define BAD_BLK_BMP_ADDR	(GC_BUF_ADDR + GC_BUF_BYTES)				// bitmap of initial bad blocks
#define BAD_BLK_BMP_BYTES	(((NUM_VBLKS / 8) + DRAM_ECC_UNIT - 1) / DRAM_ECC_UNIT * DRAM_ECC_UNIT)

/******** FTL metadata ********/

//------------------------------
// 1. address mapping information
//------------------------------
// map block mapping table
// NOTE:
//   vbn #0 : super block
// misc blk
//   vbn #1: maintain misc. DRAM metadata
// map blk
//   vbn #2: maintain data/log/isol/free BMT
//   vbn #3: maintain log page mapping hash table
//   vbn #4: bitmap info, block age
#define MAP_BLK_PER_BANK    3
#define NUM_MAP_BLK         (MAP_BLK_PER_BANK * NUM_BANKS)

// total BMT bytes
// JJ
#define FTL_BMT_BYTES       ((DATA_PMT_BYTES + DELTA_PMT_BYTES + RSRV_BMT_BYTES + BYTES_PER_SECTOR - 1) / BYTES_PER_SECTOR * BYTES_PER_SECTOR)

// data block mapping table
// JJ
#define NUM_RSRV_BLK			((NUM_RSRV_BLK + NUM_BANKS - 1) / NUM_BANKS)
//initially, all data blks are rsrv blk
#define RSRV_BLK_PER_BANK	(VBLKS_PER_BANK - 1 - 1 - MAP_BLK_PER_BANK)
#define RSRV_BMT_ADDR		(DELTA_PMT_ADDR + DELTA_PMT_BYTES)
#define RSRV_BMT_BYTES      ((NUM_BANKS * RSRV_BLK_PER_BANK * sizeof(UINT16) + DRAM_ECC_UNIT - 1) / DRAM_ECC_UNIT * DRAM_ECC_UNIT)

// non-volatile metadata structure (SRAM)
typedef struct _misc_metadata
{
    UINT32 cur_miscblk_vpn; // vblock #1 (fixed block)
    UINT32 cur_mapblk_vpn[MAP_BLK_PER_BANK];

    UINT32 rsrv_blk_cnt;
    UINT32 rsrv_list_head;
    UINT32 rsrv_list_tail;
}misc_metadata; // per bank

///////////////////////////////
// FTL public functions
///////////////////////////////

void ftl_open(void);
void ftl_read(UINT32 const lba, UINT32 const num_sectors);
void ftl_write(UINT32 const lba, UINT32 const num_sectors);
void ftl_test_write(UINT32 const lba, UINT32 const num_sectors);
void ftl_flush(void);
void ftl_isr(void);

#endif //FTL_H
