/*
 * delta.h
 *
 *  Created on: 2013. 5. 28.
 *      Author: woojj
 */

#ifndef DELTA_H_
#define DELTA_H_

#define SLRU_SIZE					128
#define PROTECTED_SEG_SIZE		64
#define PROBATIONAL_SEG_SIZE	(SLRU_SIZE - PROTECTED_SEG_SIZE)

//data blk map may not be needed
/*
#define _NUM_DATA_BLK		((NUM_LPAGES + PAGES_PER_BLK - 1) / PAGES_PER_BLK)
#define NUM_ADDITIONAL_BLK	50
#define NUM_DATA_BLK			(_NUM_DATA_BLK + NUM_ADDITIONAL_BLK)
*/

#define NUM_MAX_ORI_PAGES				NUM_LPAGES
#define NUM_MAX_ORI_PAGES_PER_BANK		(NUM_MAX_ORI_PAGES / NUM_BANKS)

#define DATA_PMT_ADDR		(BAD_BLK_BMP_ADDR + BAD_BLK_BMP_BYTES)
#define DATA_PMT_BYTES      ((NUM_MAX_ORI_PAGES * sizeof(UINT32) + DRAM_ECC_UNIT - 1) / DRAM_ECC_UNIT * DRAM_ECC_UNIT)

#define NUM_MAX_DELTA_PAGES				(NUM_DTA_BLK * PAGES_BLK - NUM_MAX_ORI_PAGES)
#define NUM_MAX_DELTA_PAGES_PER_BANK	(NUM_MAX_DELTA_PAGES / NUM_BANKS)

// static hash library
#include "shashtbl.h"

#define HASH_BUCKET_SIZE    (NUM_MAX_DELTA_PAGES_PER_BANK >> 2)
#define HASH_NODE_BYTES_PER_BANK (NUM_MAX_DELTA_PAGES_PER_BANK * sizeof(hashnode))
#define HASH_BUCKET_BYTES_PER_BANK (HASH_BUCKET_SIZE * sizeof(hashnode_ptr))

typedef struct
{
	UINT32 PPA_old;
} DataMap;

typedef struct _SLRU_node
{
	UINT32 LPA;
	UINT32 PPA_old;
	UINT32 PPA_delta;
	struct SLRU_node *next;
} SLRU_node;

typedef struct
{
	SLRU_node *protected_head;
	SLRU_node *protected_tail;
	SLRU_node *probationary_head;
	SLRU_node *probationary_tail;
} SLRU_list;


#endif /* DELTA_H_ */
