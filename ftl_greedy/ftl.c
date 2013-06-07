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
// GreedyFTL source file
//
// Author; Sang-Phil Lim (SKKU VLDB Lab.)
//
// - support POR
//  + fixed metadata area (Misc. block/Map block)
//  + logging entire FTL metadata when each ATA commands(idle/ready/standby) was issued
//

#include "jasmine.h"
#include "lzf.h"

//----------------------------------
// macro
//----------------------------------
#define VC_MAX              0xCDCD
#define MISCBLK_VBN         0x1 // vblock #1 <- misc metadata
#define MAPBLKS_PER_BANK    (((PAGE_MAP_BYTES / NUM_BANKS) + BYTES_PER_PAGE - 1) / BYTES_PER_PAGE)
#define META_BLKS_PER_BANK  (1 + 1 + MAPBLKS_PER_BANK) // include block #0, misc block

// the number of sectors of misc. metadata info.
#define NUM_MISC_META_SECT  ((sizeof(misc_metadata) + BYTES_PER_SECTOR - 1)/ BYTES_PER_SECTOR)
#define NUM_VCOUNT_SECT     ((VBLKS_PER_BANK * sizeof(UINT16) + BYTES_PER_SECTOR - 1) / BYTES_PER_SECTOR)
#define NUM_COMP_CLUSTER		16
#define CLUSTER_SIZE			(BYTES_PER_PAGE / NUM_COMP_CLUSTER)
#define ALIGN32(data)			((data + sizeof(UINT32) - 1) / sizeof(UINT32) * sizeof(UINT32))
#define MAX_COMPRESS_SIZE		(CLUSTER_SIZE / 2)				//2048		//MAX COMPRESS SIZE : 2K(50%)

/////////////////////////////////////////////

#define VAL	0x7fff
#define INVAL	0x8000
#define MAX_DELTAS_PER_PAGE			10

//----------------------------------
// metadata structure
//----------------------------------
typedef struct _ftl_statistics
{
	UINT32 gc_cnt;
	UINT32 page_wcount; // page write count
}ftl_statistics;

typedef struct _misc_metadata
{
	UINT32 cur_write_vpn; // physical page for new write
	UINT32 cur_miscblk_vpn; // current write vpn for logging the misc. metadata
	UINT32 cur_mapblk_vpn[MAPBLKS_PER_BANK]; // current write vpn for logging the age mapping info.
	UINT32 gc_vblock; // vblock number for garbage collection
	UINT32 free_blk_cnt; // total number of free block count
	UINT32 lpn_list_of_cur_vblock[PAGES_PER_BLK]; // logging lpn list of current write vblock for GC
}misc_metadata; // per bank

//----------------------------------
// FTL metadata (maintain in SRAM)
//----------------------------------
static misc_metadata  g_misc_meta[NUM_BANKS];
static ftl_statistics g_ftl_statistics[NUM_BANKS];
static UINT32		  g_bad_blk_count[NUM_BANKS];

// SATA read/write buffer pointer id
UINT32 				  g_ftl_read_buf_id;
UINT32 				  g_ftl_write_buf_id;

UINT32 g_delta_pmt_pointer;
UINT32 next_delta_offset[NUM_BANKS];						//next delta offset
UINT32 next_delta_meta[NUM_BANKS];						//next delta metadata

//----------------------------------
// NAND layout
//----------------------------------
// block #0: scan list, firmware binary image, etc.
// block #1: FTL misc. metadata
// block #2 ~ #31: page mapping table
// block #32: a free block for gc
// block #33~: user data blocks

//----------------------------------
// macro functions
//----------------------------------
#define is_full_all_blks(bank)  (g_misc_meta[bank].free_blk_cnt == 1)
#define inc_full_blk_cnt(bank)  (g_misc_meta[bank].free_blk_cnt--)
#define dec_full_blk_cnt(bank)  (g_misc_meta[bank].free_blk_cnt++)
#define inc_mapblk_vpn(bank, mapblk_lbn)    (g_misc_meta[bank].cur_mapblk_vpn[mapblk_lbn]++)
#define inc_miscblk_vpn(bank)               (g_misc_meta[bank].cur_miscblk_vpn++)

// page-level striping technique (I/O parallelism)
#define get_num_bank(lpn)             ((lpn) % NUM_BANKS)
#define get_bad_blk_cnt(bank)         (g_bad_blk_count[bank])
#define get_cur_write_vpn(bank)       (g_misc_meta[bank].cur_write_vpn)
#define set_new_write_vpn(bank, vpn)  (g_misc_meta[bank].cur_write_vpn = vpn)
#define get_gc_vblock(bank)           (g_misc_meta[bank].gc_vblock)
#define set_gc_vblock(bank, vblock)   (g_misc_meta[bank].gc_vblock = vblock)
#define set_lpn(bank, page_num, lpn)  (g_misc_meta[bank].lpn_list_of_cur_vblock[page_num] = lpn)
#define get_lpn(bank, page_num)       (g_misc_meta[bank].lpn_list_of_cur_vblock[page_num])
#define get_miscblk_vpn(bank)         (g_misc_meta[bank].cur_miscblk_vpn)
#define set_miscblk_vpn(bank, vpn)    (g_misc_meta[bank].cur_miscblk_vpn = vpn)
#define get_mapblk_vpn(bank, mapblk_lbn)      (g_misc_meta[bank].cur_mapblk_vpn[mapblk_lbn])
#define set_mapblk_vpn(bank, mapblk_lbn, vpn) (g_misc_meta[bank].cur_mapblk_vpn[mapblk_lbn] = vpn)
#define CHECK_LPAGE(lpn)              ASSERT((lpn) < NUM_LPAGES)
#define CHECK_VPAGE(vpn)              ASSERT((vpn) < (VBLKS_PER_BANK * PAGES_PER_BLK))

///////////////////////////////////////
#define get_ppa_delta(LPA)		get_delta_ppa(get_delta_map_offset(LPA))
#define get_delta_ppa(OFFSET)	read_dram_32(DELTA_PMT_ADDR + sizeof(UINT32) * (OFFSET * 2 + 1))
#define get_delta_lpa(OFFSET)	read_dram_32(DELTA_PMT_ADDR + sizeof(UINT32) * OFFSET * 2)
#define set_delta_ppa(OFFSET, PPA)	write_dram_32(DELTA_PMT_ADDR + sizeof(UINT32) * (OFFSET * 2 + 1), PPA)
#define set_delta_lpa(OFFSET, LPA)	write_dram_32(DELTA_PMT_ADDR + sizeof(UINT32) * OFFSET * 2, LPA)
#define get_pbn(ppa)		((ppa) / PAGES_PER_BLK)
#define get_offset(ppa)		((ppa) % PAGES_PER_BLK)

//----------------------------------
// FTL internal function prototype
//----------------------------------
static void   format(void);
static void   write_format_mark(void);
static void   sanity_check(void);
static void   load_pmap_table(void);
static void   load_misc_metadata(void);
static void   init_metadata_sram(void);
static void   load_metadata(void);
static void   logging_pmap_table(void);
static void   logging_misc_metadata(void);
static void   write_page(UINT32 const lpn, UINT32 const sect_offset, UINT32 const num_sectors);
static void   set_vpn(UINT32 const lpn, UINT32 const vpn);
static void   garbage_collection(UINT32 const bank);
static void   set_vcount(UINT32 const bank, UINT32 const vblock, UINT32 const vcount);
static BOOL32 is_bad_block(UINT32 const bank, UINT32 const vblock);
static BOOL32 check_format_mark(void);
static UINT32 get_vcount(UINT32 const bank, UINT32 const vblock);
static UINT32 get_vpn(UINT32 const lpn);
static UINT32 get_vt_vblock(UINT32 const bank);
static UINT32 assign_new_write_vpn(UINT32 const bank);

/////////////////////////////////////
static UINT32 set_valid_ppa(UINT32 const ppa);		//set valid ppa
static UINT32 set_invalid_ppa(UINT32 const ppa);		//set invalid ppa
static BOOL32 is_valid_ppa(UINT32 const ppa);		//is valid ppa?
static UINT32 get_delta_map_offset(UINT32 const lpa);
static void merge(UINT32 const bank, UINT32 const lpa, UINT32 const ppa_delta, UINT32 const buf_ptr);
static void read_from_delta(UINT32 const bank, UINT32 const lpa, UINT32 const delta_ppa, UINT32 const buf_addr);		//read delta to buf_addr
static BOOL32 compress(UINT32 const buf_data, UINT32 const buf_write);
static UINT32 get_next_delta_table_space(UINT32 const bank, UINT32 const lpa);
static void write_to_delta(UINT32 const bank, UINT32 const lpa, UINT32 const buf_addr);		//write to delta write buffer
static void xor_buffer(UINT32 const src0, UINT32 const src1, UINT32 const dst);
static UINT32 find_delta_data(UINT32 const buf_ptr, UINT32 const delta_ppa);		//find delta data in temp_buffer;
static void _lzf_decompress (UINT32 const in_data_base, UINT32 const out_data_base);		//decompress data
static UINT32 _lzf_compress (UINT32 const in_data_base, UINT32 const out_data_base);		//compress by lzf
static UINT32 is_remain_delta_buffer(UINT32 const bank, UINT32 const cs);	//is remain in delta_write_buffer?

static void sanity_check(void)
{
	UINT32 dram_requirement = RD_BUF_BYTES + WR_BUF_BYTES + COPY_BUF_BYTES + FTL_BUF_BYTES
			+ HIL_BUF_BYTES + TEMP_BUF_BYTES + BAD_BLK_BMP_BYTES + PAGE_MAP_BYTES + VCOUNT_BYTES;

	if ((dram_requirement > DRAM_SIZE) || // DRAM metadata size check
			(sizeof(misc_metadata) > BYTES_PER_PAGE)) // misc metadata size check
	{
		led_blink();
		while (1);
	}
}
static void build_bad_blk_list(void)
{
	UINT32 bank, num_entries, result, vblk_offset;
	scan_list_t* scan_list = (scan_list_t*) TEMP_BUF_ADDR;

	mem_set_dram(BAD_BLK_BMP_ADDR, NULL, BAD_BLK_BMP_BYTES);

	disable_irq();

	flash_clear_irq();

	for (bank = 0; bank < NUM_BANKS; bank++)
	{
		SETREG(FCP_CMD, FC_COL_ROW_READ_OUT);
		SETREG(FCP_BANK, REAL_BANK(bank));
		SETREG(FCP_OPTION, FO_E);
		SETREG(FCP_DMA_ADDR, (UINT32) scan_list);
		SETREG(FCP_DMA_CNT, SCAN_LIST_SIZE);
		SETREG(FCP_COL, 0);
		SETREG(FCP_ROW_L(bank), SCAN_LIST_PAGE_OFFSET);
		SETREG(FCP_ROW_H(bank), SCAN_LIST_PAGE_OFFSET);

		SETREG(FCP_ISSUE, NULL);
		while ((GETREG(WR_STAT) & 0x00000001) != 0);
		while (BSP_FSM(bank) != BANK_IDLE);

		num_entries = NULL;
		result = OK;

		if (BSP_INTR(bank) & FIRQ_DATA_CORRUPT)
		{
			result = FAIL;
		}
		else
		{
			UINT32 i;

			num_entries = read_dram_16(&(scan_list->num_entries));

			if (num_entries > SCAN_LIST_ITEMS)
			{
				result = FAIL;
			}
			else
			{
				for (i = 0; i < num_entries; i++)
				{
					UINT16 entry = read_dram_16(scan_list->list + i);
					UINT16 pblk_offset = entry & 0x7FFF;

					if (pblk_offset == 0 || pblk_offset >= PBLKS_PER_BANK)
					{
#if OPTION_REDUCED_CAPACITY == FALSE
						result = FAIL;
#endif
					}
					else
					{
						write_dram_16(scan_list->list + i, pblk_offset);
					}
				}
			}
		}

		if (result == FAIL)
		{
			num_entries = 0;  // We cannot trust this scan list. Perhaps a software bug.
		}
		else
		{
			write_dram_16(&(scan_list->num_entries), 0);
		}

		g_bad_blk_count[bank] = 0;

		for (vblk_offset = 1; vblk_offset < VBLKS_PER_BANK; vblk_offset++)
		{
			BOOL32 bad = FALSE;

#if OPTION_2_PLANE
			{
				UINT32 pblk_offset;

				pblk_offset = vblk_offset * NUM_PLANES;

				// fix bug@jasmine v.1.1.0
				if (mem_search_equ_dram(scan_list, sizeof(UINT16), num_entries + 1, pblk_offset) < num_entries + 1)
				{
					bad = TRUE;
				}

				pblk_offset = vblk_offset * NUM_PLANES + 1;

				// fix bug@jasmine v.1.1.0
				if (mem_search_equ_dram(scan_list, sizeof(UINT16), num_entries + 1, pblk_offset) < num_entries + 1)
				{
					bad = TRUE;
				}
			}
#else
			{
				// fix bug@jasmine v.1.1.0
				if (mem_search_equ_dram(scan_list, sizeof(UINT16), num_entries + 1, vblk_offset) < num_entries + 1)
				{
					bad = TRUE;
				}
			}
#endif

			if (bad)
			{
				g_bad_blk_count[bank]++;
				set_bit_dram(BAD_BLK_BMP_ADDR + bank*(VBLKS_PER_BANK/8 + 1), vblk_offset);
			}
		}
	}
}

void ftl_open(void)
{
	// debugging example 1 - use breakpoint statement!
	/* *(UINT32*)0xFFFFFFFE = 10; */

	/* UINT32 volatile g_break = 0; */
	/* while (g_break == 0); */

	led(0);
	sanity_check();
	//----------------------------------------
	// read scan lists from NAND flash
	// and build bitmap of bad blocks
	//----------------------------------------
	build_bad_blk_list();

	//----------------------------------------
	// If necessary, do low-level format
	// format() should be called after loading scan lists, because format() calls is_bad_block().
	//----------------------------------------
	/* 	if (check_format_mark() == FALSE) */
	if (TRUE)
	{
		uart_print("do format");
		format();
		uart_print("end format");
	}
	// load FTL metadata
	else
	{
		load_metadata();
	}
	g_ftl_read_buf_id = 0;
	g_ftl_write_buf_id = 0;

	// This example FTL can handle runtime bad block interrupts and read fail (uncorrectable bit errors) interrupts
	flash_clear_irq();

	SETREG(INTR_MASK, FIRQ_DATA_CORRUPT | FIRQ_BADBLK_L | FIRQ_BADBLK_H);
	SETREG(FCONF_PAUSE, FIRQ_DATA_CORRUPT | FIRQ_BADBLK_L | FIRQ_BADBLK_H);

	enable_irq();
}
void ftl_flush(void)
{
	/* ptimer_start(); */
	logging_pmap_table();
	logging_misc_metadata();
	/* ptimer_stop_and_uart_print(); */
}
// Testing FTL protocol APIs
void ftl_test_write(UINT32 const lba, UINT32 const num_sectors)
{
	ASSERT(lba + num_sectors <= NUM_LSECTORS);
	ASSERT(num_sectors > 0);

	ftl_write(lba, num_sectors);
}
void ftl_read(UINT32 const lba, UINT32 const num_sectors)
{
	UINT32 remain_sects, num_sectors_to_read;
	UINT32 lpn, sect_offset;
	UINT32 bank, vpn;

	lpn          = lba / SECTORS_PER_PAGE;
	sect_offset  = lba % SECTORS_PER_PAGE;
	remain_sects = num_sectors;

	while (remain_sects != 0)
	{
		if ((sect_offset + remain_sects) < SECTORS_PER_PAGE)
		{
			num_sectors_to_read = remain_sects;
		}
		else
		{
			num_sectors_to_read = SECTORS_PER_PAGE - sect_offset;
		}
		bank = get_num_bank(lpn); // page striping
		vpn  = get_vpn(lpn);
		CHECK_VPAGE(vpn);

		if (vpn != NULL)
		{
			//?댁쟻???덉쓬
			/* ?명?媛 ?덉쑝硫?			 * 	?명?瑜??쎌뼱????⑹튇?ㅼ쓬 由щ뱶踰꾪띁???섍?
			 * ?명?媛 ?놁쑝硫?			 * 	洹몃깷 由ы꽩
			 */
			if(is_valid_ppa(vpn) == FALSE)
			{
				//?명?媛 ?덉쓬
				merge(bank, lpn, get_ppa_delta(lpn), RD_BUF_PTR(g_ftl_read_buf_id));
			}
			else
			{
				//?명?媛 ?놁쓬
				nand_page_read(bank,
						vpn / PAGES_PER_BLK,
						vpn % PAGES_PER_BLK,
						RD_BUF_PTR(g_ftl_read_buf_id));
			}
			g_ftl_read_buf_id = (g_ftl_read_buf_id + 1) % NUM_RD_BUFFERS;
		}
		// The host is requesting to read a logical page that has never been written to.
		else
		{
			UINT32 next_read_buf_id = (g_ftl_read_buf_id + 1) % NUM_RD_BUFFERS;

#if OPTION_FTL_TEST == 0
			while (next_read_buf_id == GETREG(SATA_RBUF_PTR));	// wait if the read buffer is full (slow host)
#endif

			// fix bug @ v.1.0.6
			// Send 0xFF...FF to host when the host request to read the sector that has never been written.
			// In old version, for example, if the host request to read unwritten sector 0 after programming in sector 1, Jasmine would send 0x00...00 to host.
			// However, if the host already wrote to sector 1, Jasmine would send 0xFF...FF to host when host request to read sector 0. (ftl_read() in ftl_xxx/ftl.c)
			mem_set_dram(RD_BUF_PTR(g_ftl_read_buf_id) + sect_offset*BYTES_PER_SECTOR,
					0x0, num_sectors_to_read*BYTES_PER_SECTOR);

			flash_finish();

			SETREG(BM_STACK_RDSET, next_read_buf_id);	// change bm_read_limit
			SETREG(BM_STACK_RESET, 0x02);				// change bm_read_limit

			g_ftl_read_buf_id = next_read_buf_id;
		}
		sect_offset   = 0;
		remain_sects -= num_sectors_to_read;
		lpn++;
	}
}
void ftl_write(UINT32 const lba, UINT32 const num_sectors)
{
	UINT32 remain_sects, num_sectors_to_write;
	UINT32 lpn, sect_offset;

	lpn          = lba / SECTORS_PER_PAGE;
	sect_offset  = lba % SECTORS_PER_PAGE;
	remain_sects = num_sectors;

	while (remain_sects != 0)
	{
		if ((sect_offset + remain_sects) < SECTORS_PER_PAGE)
		{
			num_sectors_to_write = remain_sects;
		}
		else
		{
			num_sectors_to_write = SECTORS_PER_PAGE - sect_offset;
		}
		// single page write individually
		write_page(lpn, sect_offset, num_sectors_to_write);

		sect_offset   = 0;
		remain_sects -= num_sectors_to_write;
		lpn++;
	}
}
static void write_page(UINT32 const lpn, UINT32 const sect_offset, UINT32 const num_sectors)
{
	CHECK_LPAGE(lpn);
	ASSERT(sect_offset < SECTORS_PER_PAGE);
	ASSERT(num_sectors > 0 && num_sectors <= SECTORS_PER_PAGE);

	UINT32 bank, old_vpn, new_vpn;
	UINT32 vblock, page_num, page_offset, column_cnt;

	bank        = get_num_bank(lpn); // page striping
	page_offset = sect_offset;
	column_cnt  = num_sectors;

	old_vpn  = get_vpn(lpn);
	/*
	 * old_vpn
	 * 泥섏쓬?곕㈃ ?먯씠怨?	 * ?명?媛 ?덉쑝硫??몃갭由щ뱶
	 * ?명?媛 ?놁쑝硫?諛몃━?쒓쿋吏
	 */

	g_ftl_statistics[bank].page_wcount++;

	// if old data already exist,
	if (old_vpn != NULL)
	{
		old_vpn = set_valid_ppa(old_vpn);

		vblock   = old_vpn / PAGES_PER_BLK;
		page_num = old_vpn % PAGES_PER_BLK;
		nand_page_read(bank, vblock, old_vpn % PAGES_PER_BLK, FTL_BUF(bank));

		//--------------------------------------------------------------------------------------
		// `Partial programming'
		// we could not determine whether the new data is loaded in the SATA write buffer.
		// Thus, read the left/right hole sectors of a valid page and copy into the write buffer.
		// And then, program whole valid data
		//--------------------------------------------------------------------------------------
		if (num_sectors != SECTORS_PER_PAGE)
		{
			// read `left hole sectors'
			if (page_offset != 0)
			{
				mem_copy(WR_BUF_PTR(g_ftl_write_buf_id), FTL_BUF(bank), page_offset * BYTES_PER_SECTOR);
			}
			// read `right hole sectors'
			if ((page_offset + column_cnt) < SECTORS_PER_PAGE)
			{
				UINT32 const rhole_base = (page_offset + num_sectors) * BYTES_PER_SECTOR;
				mem_copy(WR_BUF_PTR(g_ftl_write_buf_id) + rhole_base, FTL_BUF(bank), BYTES_PER_PAGE - rhole_base);
			}
		}
		// full page write
		page_offset = 0;
		column_cnt  = SECTORS_PER_PAGE;

		//BOOL32 comp_success = compress(TEMP_BUF_PTR(0), WR_BUF_PTR(g_ftl_write_buf_id));
		//if(comp_success == TRUE)
		if( FALSE )
		{
			uart_printf("evict 3\n");
			//?뺤텞 ?깃났
			//?곗씠?곕ℓ?묓뀒?대툝 ?낅뜲?댄듃
			old_vpn = set_invalid_ppa(old_vpn);
			set_vpn(lpn, old_vpn);
			//?명?留ㅽ븨 ???꾩튂?좎젙
			UINT32 delta_page_map_offset = get_next_delta_table_space(bank, lpn);

			/*
			 * ?명? 留ㅽ븨 ?뚯씠釉붿뿉?ㅺ? ?뷀듃由??낅뜲?댄듃
			 * lpa??洹몃깷 ??二쇨퀬
			 * ppa??0???
			 * 0? ?명? 踰꾪띁???덈떎???섎?
			 * 洹몃━怨??뺤텞??寃껋? write to delta
			 */
			set_delta_lpa(delta_page_map_offset, lpn);
			set_delta_ppa(delta_page_map_offset, 0);
			/*
			 * ?명????곕뒗 ?⑥닔
			 * write_to_delta
			 * ?몄옄 : bank, lpa, buf_ptr
			 * buf_ptr? ?뺤텞???덉씠 ??λ맂 踰꾪띁
			 * ?뺤텞???덉쓣 ?명?踰꾪띁??吏묒뼱?ｌ쓬
			 * 苑됱감硫??뚯븘???몃뱶???
			 */
			write_to_delta(bank, lpn, TEMP_BUF_PTR(2));
			return;
		}
		else
		{
			// invalid old page (decrease vcount)
			set_vcount(bank, vblock, get_vcount(bank, vblock) - 1);
		}
	}
	else
	{
		//泥섏쓬 ?
		if (num_sectors != SECTORS_PER_PAGE)
		{
			if(page_offset != 0)
				mem_set_dram(WR_BUF_PTR(g_ftl_write_buf_id), 0, page_offset * BYTES_PER_SECTOR);
			if((page_offset + num_sectors) < SECTORS_PER_PAGE)
			{
				UINT32 const rhole_base = (page_offset + num_sectors) * BYTES_PER_SECTOR;
				mem_set_dram(WR_BUF_PTR(g_ftl_write_buf_id) + rhole_base, 0, BYTES_PER_PAGE - rhole_base);
			}
		}
	}
	new_vpn  = assign_new_write_vpn(bank);
	vblock   = new_vpn / PAGES_PER_BLK;
	page_num = new_vpn % PAGES_PER_BLK;
	ASSERT(get_vcount(bank,vblock) < (PAGES_PER_BLK - 1));

	// write new data (make sure that the new data is ready in the write buffer frame)
	// (c.f FO_B_SATA_W flag in flash.h)
	nand_page_program_from_host(bank,
			vblock,
			page_num);
	// update metadata
	set_lpn(bank, page_num, lpn);
	set_vpn(lpn, new_vpn);
	set_vcount(bank, vblock, get_vcount(bank, vblock) + 1);
}
// get vpn from PAGE_MAP
static UINT32 get_vpn(UINT32 const lpn)
{
	CHECK_LPAGE(lpn);
	return read_dram_32(PAGE_MAP_ADDR + lpn * sizeof(UINT32));
}
// set vpn to PAGE_MAP
static void set_vpn(UINT32 const lpn, UINT32 const vpn)
{
	CHECK_LPAGE(lpn);
	ASSERT(vpn >= (META_BLKS_PER_BANK * PAGES_PER_BLK) && vpn < (VBLKS_PER_BANK * PAGES_PER_BLK));

	write_dram_32(PAGE_MAP_ADDR + lpn * sizeof(UINT32), vpn);
}
// get valid page count of vblock
static UINT32 get_vcount(UINT32 const bank, UINT32 const vblock)
{
	UINT32 vcount;

	ASSERT(bank < NUM_BANKS);
	ASSERT((vblock >= META_BLKS_PER_BANK) && (vblock < VBLKS_PER_BANK));

	vcount = read_dram_16(VCOUNT_ADDR + (((bank * VBLKS_PER_BANK) + vblock) * sizeof(UINT16)));
	ASSERT((vcount < PAGES_PER_BLK) || (vcount == VC_MAX));

	return vcount;
}
// set valid page count of vblock
static void set_vcount(UINT32 const bank, UINT32 const vblock, UINT32 const vcount)
{
	ASSERT(bank < NUM_BANKS);
	ASSERT((vblock >= META_BLKS_PER_BANK) && (vblock < VBLKS_PER_BANK));
	ASSERT((vcount < PAGES_PER_BLK) || (vcount == VC_MAX));

	write_dram_16(VCOUNT_ADDR + (((bank * VBLKS_PER_BANK) + vblock) * sizeof(UINT16)), vcount);
}
static UINT32 assign_new_write_vpn(UINT32 const bank)
{
	ASSERT(bank < NUM_BANKS);

	UINT32 write_vpn;
	UINT32 vblock;

	write_vpn = get_cur_write_vpn(bank);
	vblock    = write_vpn / PAGES_PER_BLK;

	// NOTE: if next new write page's offset is
	// the last page offset of vblock (i.e. PAGES_PER_BLK - 1),
	if ((write_vpn % PAGES_PER_BLK) == (PAGES_PER_BLK - 2))
	{
		// then, because of the flash controller limitation
		// (prohibit accessing a spare area (i.e. OOB)),
		// thus, we persistenly write a lpn list into last page of vblock.
		mem_copy(FTL_BUF(bank), g_misc_meta[bank].lpn_list_of_cur_vblock, sizeof(UINT32) * PAGES_PER_BLK);
		// fix minor bug
		nand_page_ptprogram(bank, vblock, PAGES_PER_BLK - 1, 0,
				((sizeof(UINT32) * PAGES_PER_BLK + BYTES_PER_SECTOR - 1 ) / BYTES_PER_SECTOR), FTL_BUF(bank));

		mem_set_sram(g_misc_meta[bank].lpn_list_of_cur_vblock, 0x00000000, sizeof(UINT32) * PAGES_PER_BLK);

		inc_full_blk_cnt(bank);

		// do garbage collection if necessary
		if (is_full_all_blks(bank))
		{
			garbage_collection(bank);
			return get_cur_write_vpn(bank);
		}
		do
		{
			vblock++;

			ASSERT(vblock != VBLKS_PER_BANK);
		}while (get_vcount(bank, vblock) == VC_MAX);
	}
	// write page -> next block
	if (vblock != (write_vpn / PAGES_PER_BLK))
	{
		write_vpn = vblock * PAGES_PER_BLK;
	}
	else
	{
		write_vpn++;
	}
	set_new_write_vpn(bank, write_vpn);

	return write_vpn;
}
static BOOL32 is_bad_block(UINT32 const bank, UINT32 const vblk_offset)
{
	if (tst_bit_dram(BAD_BLK_BMP_ADDR + bank*(VBLKS_PER_BANK/8 + 1), vblk_offset) == FALSE)
	{
		return FALSE;
	}
	return TRUE;
}
//------------------------------------------------------------
// if all blocks except one free block are full,
// do garbage collection for making at least one free page
//-------------------------------------------------------------
static void garbage_collection(UINT32 const bank)
{
	ASSERT(bank < NUM_BANKS);
	g_ftl_statistics[bank].gc_cnt++;

	UINT32 src_lpn;
	UINT32 vt_vblock;
	UINT32 free_vpn;
	UINT32 vcount; // valid page count in victim block
	UINT32 src_page;
	UINT32 gc_vblock;

	g_ftl_statistics[bank].gc_cnt++;

	vt_vblock = get_vt_vblock(bank);   // get victim block
	vcount    = get_vcount(bank, vt_vblock);
	gc_vblock = get_gc_vblock(bank);
	free_vpn  = gc_vblock * PAGES_PER_BLK;

	/*     uart_printf("garbage_collection bank %d, vblock %d",bank, vt_vblock); */

	ASSERT(vt_vblock != gc_vblock);
	ASSERT(vt_vblock >= META_BLKS_PER_BANK && vt_vblock < VBLKS_PER_BANK);
	ASSERT(vcount < (PAGES_PER_BLK - 1));
	ASSERT(get_vcount(bank, gc_vblock) == VC_MAX);
	ASSERT(!is_bad_block(bank, gc_vblock));

	// 1. load p2l list from last page offset of victim block (4B x PAGES_PER_BLK)
	// fix minor bug
	nand_page_ptread(bank, vt_vblock, PAGES_PER_BLK - 1, 0,
			((sizeof(UINT32) * PAGES_PER_BLK + BYTES_PER_SECTOR - 1 ) / BYTES_PER_SECTOR), FTL_BUF(bank), RETURN_WHEN_DONE);
	mem_copy(g_misc_meta[bank].lpn_list_of_cur_vblock, FTL_BUF(bank), sizeof(UINT32) * PAGES_PER_BLK);
	// 2. copy-back all valid pages to free space
	for (src_page = 0; src_page < (PAGES_PER_BLK - 1); src_page++)
	{
		// get lpn of victim block from a read lpn list
		src_lpn = get_lpn(bank, src_page);
		CHECK_VPAGE(get_vpn(src_lpn));

		// determine whether the page is valid or not
		if (get_vpn(src_lpn) !=
				((vt_vblock * PAGES_PER_BLK) + src_page))
		{
			// invalid page
			continue;
		}
		ASSERT(get_lpn(bank, src_page) != INVALID);
		CHECK_LPAGE(src_lpn);
		// if the page is valid,
		// then do copy-back op. to free space
		nand_page_copyback(bank,
				vt_vblock,
				src_page,
				free_vpn / PAGES_PER_BLK,
				free_vpn % PAGES_PER_BLK);
		ASSERT((free_vpn / PAGES_PER_BLK) == gc_vblock);
		// update metadata
		set_vpn(src_lpn, free_vpn);
		set_lpn(bank, (free_vpn % PAGES_PER_BLK), src_lpn);

		free_vpn++;
	}
#if OPTION_ENABLE_ASSERT
	if (vcount == 0)
	{
		ASSERT(free_vpn == (gc_vblock * PAGES_PER_BLK));
	}
#endif
	// 3. erase victim block
	nand_block_erase(bank, vt_vblock);
	ASSERT((free_vpn % PAGES_PER_BLK) < (PAGES_PER_BLK - 2));
	ASSERT((free_vpn % PAGES_PER_BLK == vcount));

	/*     uart_printf("gc page count : %d", vcount); */

	// 4. update metadata
	set_vcount(bank, vt_vblock, VC_MAX);
	set_vcount(bank, gc_vblock, vcount);
	set_new_write_vpn(bank, free_vpn); // set a free page for new write
	set_gc_vblock(bank, vt_vblock); // next free block (reserve for GC)
	dec_full_blk_cnt(bank); // decrease full block count
	/* uart_print("garbage_collection end"); */
}
//-------------------------------------------------------------
// Victim selection policy: Greedy
//
// Select the block which contain minumum valid pages
//-------------------------------------------------------------
static UINT32 get_vt_vblock(UINT32 const bank)
{
	ASSERT(bank < NUM_BANKS);

	UINT32 vblock;

	// search the block which has mininum valid pages
	vblock = mem_search_min_max(VCOUNT_ADDR + (bank * VBLKS_PER_BANK * sizeof(UINT16)),
			sizeof(UINT16),
			VBLKS_PER_BANK,
			MU_CMD_SEARCH_MIN_DRAM);

	ASSERT(is_bad_block(bank, vblock) == FALSE);
	ASSERT(vblock >= META_BLKS_PER_BANK && vblock < VBLKS_PER_BANK);
	ASSERT(get_vcount(bank, vblock) < (PAGES_PER_BLK - 1));

	return vblock;
}
static void format(void)
{
	UINT32 bank, vblock, vcount_val;

	ASSERT(NUM_MISC_META_SECT > 0);
	ASSERT(NUM_VCOUNT_SECT > 0);

	uart_printf("Total FTL DRAM metadata size: %d KB", DRAM_BYTES_OTHER / 1024);

	uart_printf("VBLKS_PER_BANK: %d", VBLKS_PER_BANK);
	uart_printf("LBLKS_PER_BANK: %d", NUM_LPAGES / PAGES_PER_BLK / NUM_BANKS);
	uart_printf("META_BLKS_PER_BANK: %d", META_BLKS_PER_BANK);

	//----------------------------------------
	// initialize DRAM metadata
	//----------------------------------------
	mem_set_dram(PAGE_MAP_ADDR, NULL, PAGE_MAP_BYTES);
	mem_set_dram(VCOUNT_ADDR, NULL, VCOUNT_BYTES);

	//----------------------------------------
	// erase all blocks except vblock #0
	//----------------------------------------
	for (vblock = MISCBLK_VBN; vblock < VBLKS_PER_BANK; vblock++)
	{
		for (bank = 0; bank < NUM_BANKS; bank++)
		{
			vcount_val = VC_MAX;
			if (is_bad_block(bank, vblock) == FALSE)
			{
				nand_block_erase(bank, vblock);
				vcount_val = 0;
			}
			write_dram_16(VCOUNT_ADDR + ((bank * VBLKS_PER_BANK) + vblock) * sizeof(UINT16),
					vcount_val);
		}
	}
	//----------------------------------------
	// initialize SRAM metadata
	//----------------------------------------
	init_metadata_sram();

	// flush metadata to NAND
	logging_pmap_table();
	logging_misc_metadata();

	write_format_mark();
	led(1);
	uart_print("format complete");
}
static void init_metadata_sram(void)
{
	UINT32 bank;
	UINT32 vblock;
	UINT32 mapblk_lbn;

	//----------------------------------------
	// initialize misc. metadata
	//----------------------------------------
	for (bank = 0; bank < NUM_BANKS; bank++)
	{
		g_misc_meta[bank].free_blk_cnt = VBLKS_PER_BANK - META_BLKS_PER_BANK;
		g_misc_meta[bank].free_blk_cnt -= get_bad_blk_cnt(bank);
		// NOTE: vblock #0,1 don't use for user space
		write_dram_16(VCOUNT_ADDR + ((bank * VBLKS_PER_BANK) + 0) * sizeof(UINT16), VC_MAX);
		write_dram_16(VCOUNT_ADDR + ((bank * VBLKS_PER_BANK) + 1) * sizeof(UINT16), VC_MAX);

		//----------------------------------------
		// assign misc. block
		//----------------------------------------
		// assumption: vblock #1 = fixed location.
		// Thus if vblock #1 is a bad block, it should be allocate another block.
		set_miscblk_vpn(bank, MISCBLK_VBN * PAGES_PER_BLK - 1);
		ASSERT(is_bad_block(bank, MISCBLK_VBN) == FALSE);

		vblock = MISCBLK_VBN;

		//----------------------------------------
		// assign map block
		//----------------------------------------
		mapblk_lbn = 0;
		while (mapblk_lbn < MAPBLKS_PER_BANK)
		{
			vblock++;
			ASSERT(vblock < VBLKS_PER_BANK);
			if (is_bad_block(bank, vblock) == FALSE)
			{
				set_mapblk_vpn(bank, mapblk_lbn, vblock * PAGES_PER_BLK);
				write_dram_16(VCOUNT_ADDR + ((bank * VBLKS_PER_BANK) + vblock) * sizeof(UINT16), VC_MAX);
				mapblk_lbn++;
			}
		}
		//----------------------------------------
		// assign free block for gc
		//----------------------------------------
		do
		{
			vblock++;
			// NOTE: free block should not be secleted as a victim @ first GC
			write_dram_16(VCOUNT_ADDR + ((bank * VBLKS_PER_BANK) + vblock) * sizeof(UINT16), VC_MAX);
			// set free block
			set_gc_vblock(bank, vblock);

			ASSERT(vblock < VBLKS_PER_BANK);
		}while(is_bad_block(bank, vblock) == TRUE);
		//----------------------------------------
		// assign free vpn for first new write
		//----------------------------------------
		do
		{
			vblock++;
			// 占쎄쑴??next vblock?븝옙苑?占쎈뜄以덌옙占쏙옙怨쀬뵠占쎄퀡占?占쏙옙?ｏ옙占쏙옙?뽰삂
			set_new_write_vpn(bank, vblock * PAGES_PER_BLK);
			ASSERT(vblock < VBLKS_PER_BANK);
		}while(is_bad_block(bank, vblock) == TRUE);
	}
}
// logging misc + vcount metadata
static void logging_misc_metadata(void)
{
	UINT32 misc_meta_bytes = NUM_MISC_META_SECT * BYTES_PER_SECTOR; // per bank
	UINT32 vcount_addr     = VCOUNT_ADDR;
	UINT32 vcount_bytes    = NUM_VCOUNT_SECT * BYTES_PER_SECTOR; // per bank
	UINT32 vcount_boundary = VCOUNT_ADDR + VCOUNT_BYTES; // entire vcount data
	UINT32 bank;

	flash_finish();

	for (bank = 0; bank < NUM_BANKS; bank++)
	{
		inc_miscblk_vpn(bank);

		// note: if misc. meta block is full, just erase old block & write offset #0
		if ((get_miscblk_vpn(bank) / PAGES_PER_BLK) != MISCBLK_VBN)
		{
			nand_block_erase(bank, MISCBLK_VBN);
			set_miscblk_vpn(bank, MISCBLK_VBN * PAGES_PER_BLK); // vpn = 128
		}
		// copy misc. metadata to FTL buffer
		mem_copy(FTL_BUF(bank), &g_misc_meta[bank], misc_meta_bytes);

		// copy vcount metadata to FTL buffer
		if (vcount_addr <= vcount_boundary)
		{
			mem_copy(FTL_BUF(bank) + misc_meta_bytes, vcount_addr, vcount_bytes);
			vcount_addr += vcount_bytes;
		}
	}
	// logging the misc. metadata to nand flash
	for (bank = 0; bank < NUM_BANKS; bank++)
	{
		nand_page_ptprogram(bank,
				get_miscblk_vpn(bank) / PAGES_PER_BLK,
				get_miscblk_vpn(bank) % PAGES_PER_BLK,
				0,
				NUM_MISC_META_SECT + NUM_VCOUNT_SECT,
				FTL_BUF(bank));
	}
	flash_finish();
}
static void logging_pmap_table(void)
{
	UINT32 pmap_addr  = PAGE_MAP_ADDR;
	UINT32 pmap_bytes = BYTES_PER_PAGE; // per bank
	UINT32 mapblk_vpn;
	UINT32 bank;
	UINT32 pmap_boundary = PAGE_MAP_ADDR + PAGE_MAP_BYTES;
	BOOL32 finished = FALSE;

	for (UINT32 mapblk_lbn = 0; mapblk_lbn < MAPBLKS_PER_BANK; mapblk_lbn++)
	{
		flash_finish();

		for (bank = 0; bank < NUM_BANKS; bank++)
		{
			if (finished)
			{
				break;
			}
			else if (pmap_addr >= pmap_boundary)
			{
				finished = TRUE;
				break;
			}
			else if (pmap_addr + BYTES_PER_PAGE >= pmap_boundary)
			{
				finished = TRUE;
				pmap_bytes = (pmap_boundary - pmap_addr + BYTES_PER_SECTOR - 1) / BYTES_PER_SECTOR * BYTES_PER_SECTOR ;
			}
			inc_mapblk_vpn(bank, mapblk_lbn);

			mapblk_vpn = get_mapblk_vpn(bank, mapblk_lbn);

			// note: if there is no free page, then erase old map block first.
			if ((mapblk_vpn % PAGES_PER_BLK) == 0)
			{
				// erase full map block
				nand_block_erase(bank, (mapblk_vpn - 1) / PAGES_PER_BLK);

				// next vpn of mapblk is offset #0
				set_mapblk_vpn(bank, mapblk_lbn, ((mapblk_vpn - 1) / PAGES_PER_BLK) * PAGES_PER_BLK);
				mapblk_vpn = get_mapblk_vpn(bank, mapblk_lbn);
			}
			// copy the page mapping table to FTL buffer
			mem_copy(FTL_BUF(bank), pmap_addr, pmap_bytes);

			// logging update page mapping table into map_block
			nand_page_ptprogram(bank,
					mapblk_vpn / PAGES_PER_BLK,
					mapblk_vpn % PAGES_PER_BLK,
					0,
					pmap_bytes / BYTES_PER_SECTOR,
					FTL_BUF(bank));
			pmap_addr += pmap_bytes;
		}
		if (finished)
		{
			break;
		}
	}
	flash_finish();
}
// load flushed FTL metadta
static void load_metadata(void)
{
	load_misc_metadata();
	load_pmap_table();
}
// misc + VCOUNT
static void load_misc_metadata(void)
{
	UINT32 misc_meta_bytes = NUM_MISC_META_SECT * BYTES_PER_SECTOR;
	UINT32 vcount_bytes    = NUM_VCOUNT_SECT * BYTES_PER_SECTOR;
	UINT32 vcount_addr     = VCOUNT_ADDR;
	UINT32 vcount_boundary = VCOUNT_ADDR + VCOUNT_BYTES;

	UINT32 load_flag = 0;
	UINT32 bank, page_num;
	UINT32 load_cnt = 0;

	flash_finish();

	disable_irq();
	flash_clear_irq();	// clear any flash interrupt flags that might have been set

	// scan valid metadata in descending order from last page offset
	for (page_num = PAGES_PER_BLK - 1; page_num != ((UINT32) -1); page_num--)
	{
		for (bank = 0; bank < NUM_BANKS; bank++)
		{
			if (load_flag & (0x1 << bank))
			{
				continue;
			}
			// read valid metadata from misc. metadata area
			nand_page_ptread(bank,
					MISCBLK_VBN,
					page_num,
					0,
					NUM_MISC_META_SECT + NUM_VCOUNT_SECT,
					FTL_BUF(bank),
					RETURN_ON_ISSUE);
		}
		flash_finish();

		for (bank = 0; bank < NUM_BANKS; bank++)
		{
			if (!(load_flag & (0x1 << bank)) && !(BSP_INTR(bank) & FIRQ_ALL_FF))
			{
				load_flag = load_flag | (0x1 << bank);
				load_cnt++;
			}
			CLR_BSP_INTR(bank, 0xFF);
		}
	}
	ASSERT(load_cnt == NUM_BANKS);

	for (bank = 0; bank < NUM_BANKS; bank++)
	{
		// misc. metadata
		mem_copy(&g_misc_meta[bank], FTL_BUF(bank), sizeof(misc_metadata));

		// vcount metadata
		if (vcount_addr <= vcount_boundary)
		{
			mem_copy(vcount_addr, FTL_BUF(bank) + misc_meta_bytes, vcount_bytes);
			vcount_addr += vcount_bytes;

		}
	}
	enable_irq();
}
static void load_pmap_table(void)
{
	UINT32 pmap_addr = PAGE_MAP_ADDR;
	UINT32 temp_page_addr;
	UINT32 pmap_bytes = BYTES_PER_PAGE; // per bank
	UINT32 pmap_boundary = PAGE_MAP_ADDR + (NUM_LPAGES * sizeof(UINT32));
	UINT32 mapblk_lbn, bank;
	BOOL32 finished = FALSE;

	flash_finish();

	for (mapblk_lbn = 0; mapblk_lbn < MAPBLKS_PER_BANK; mapblk_lbn++)
	{
		temp_page_addr = pmap_addr; // backup page mapping addr

		for (bank = 0; bank < NUM_BANKS; bank++)
		{
			if (finished)
			{
				break;
			}
			else if (pmap_addr >= pmap_boundary)
			{
				finished = TRUE;
				break;
			}
			else if (pmap_addr + BYTES_PER_PAGE >= pmap_boundary)
			{
				finished = TRUE;
				pmap_bytes = (pmap_boundary - pmap_addr + BYTES_PER_SECTOR - 1) / BYTES_PER_SECTOR * BYTES_PER_SECTOR;
			}
			// read page mapping table from map_block
			nand_page_ptread(bank,
					get_mapblk_vpn(bank, mapblk_lbn) / PAGES_PER_BLK,
					get_mapblk_vpn(bank, mapblk_lbn) % PAGES_PER_BLK,
					0,
					pmap_bytes / BYTES_PER_SECTOR,
					FTL_BUF(bank),
					RETURN_ON_ISSUE);
			pmap_addr += pmap_bytes;
		}
		flash_finish();

		pmap_bytes = BYTES_PER_PAGE;
		for (bank = 0; bank < NUM_BANKS; bank++)
		{
			if (temp_page_addr >= pmap_boundary)
			{
				break;
			}
			else if (temp_page_addr + BYTES_PER_PAGE >= pmap_boundary)
			{
				pmap_bytes = (pmap_boundary - temp_page_addr + BYTES_PER_SECTOR - 1) / BYTES_PER_SECTOR * BYTES_PER_SECTOR;
			}
			// copy page mapping table to PMAP_ADDR from FTL buffer
			mem_copy(temp_page_addr, FTL_BUF(bank), pmap_bytes);

			temp_page_addr += pmap_bytes;
		}
		if (finished)
		{
			break;
		}
	}
}
static void write_format_mark(void)
{
	// This function writes a format mark to a page at (bank #0, block #0).

#ifdef __GNUC__
	extern UINT32 size_of_firmware_image;
	UINT32 firmware_image_pages = (((UINT32) (&size_of_firmware_image)) + BYTES_PER_FW_PAGE - 1) / BYTES_PER_FW_PAGE;
#else
	extern UINT32 Image$$ER_CODE$$RO$$Length;
	extern UINT32 Image$$ER_RW$$RW$$Length;
	UINT32 firmware_image_bytes = ((UINT32) &Image$$ER_CODE$$RO$$Length) + ((UINT32) &Image$$ER_RW$$RW$$Length);
	UINT32 firmware_image_pages = (firmware_image_bytes + BYTES_PER_FW_PAGE - 1) / BYTES_PER_FW_PAGE;
#endif

	UINT32 format_mark_page_offset = FW_PAGE_OFFSET + firmware_image_pages;

	mem_set_dram(FTL_BUF_ADDR, 0, BYTES_PER_SECTOR);

	SETREG(FCP_CMD, FC_COL_ROW_IN_PROG);
	SETREG(FCP_BANK, REAL_BANK(0));
	SETREG(FCP_OPTION, FO_E | FO_B_W_DRDY);
	SETREG(FCP_DMA_ADDR, FTL_BUF_ADDR); 	// DRAM -> flash
	SETREG(FCP_DMA_CNT, BYTES_PER_SECTOR);
	SETREG(FCP_COL, 0);
	SETREG(FCP_ROW_L(0), format_mark_page_offset);
	SETREG(FCP_ROW_H(0), format_mark_page_offset);

	// At this point, we do not have to check Waiting Room status before issuing a command,
	// because we have waited for all the banks to become idle before returning from format().
	SETREG(FCP_ISSUE, NULL);

	// wait for the FC_COL_ROW_IN_PROG command to be accepted by bank #0
	while ((GETREG(WR_STAT) & 0x00000001) != 0);

	// wait until bank #0 finishes the write operation
	while (BSP_FSM(0) != BANK_IDLE);
}
static BOOL32 check_format_mark(void)
{
	// This function reads a flash page from (bank #0, block #0) in order to check whether the SSD is formatted or not.

#ifdef __GNUC__
	extern UINT32 size_of_firmware_image;
	UINT32 firmware_image_pages = (((UINT32) (&size_of_firmware_image)) + BYTES_PER_FW_PAGE - 1) / BYTES_PER_FW_PAGE;
#else
	extern UINT32 Image$$ER_CODE$$RO$$Length;
	extern UINT32 Image$$ER_RW$$RW$$Length;
	UINT32 firmware_image_bytes = ((UINT32) &Image$$ER_CODE$$RO$$Length) + ((UINT32) &Image$$ER_RW$$RW$$Length);
	UINT32 firmware_image_pages = (firmware_image_bytes + BYTES_PER_FW_PAGE - 1) / BYTES_PER_FW_PAGE;
#endif

	UINT32 format_mark_page_offset = FW_PAGE_OFFSET + firmware_image_pages;
	UINT32 temp;

	flash_clear_irq();	// clear any flash interrupt flags that might have been set

	SETREG(FCP_CMD, FC_COL_ROW_READ_OUT);
	SETREG(FCP_BANK, REAL_BANK(0));
	SETREG(FCP_OPTION, FO_E);
	SETREG(FCP_DMA_ADDR, FTL_BUF_ADDR); 	// flash -> DRAM
	SETREG(FCP_DMA_CNT, BYTES_PER_SECTOR);
	SETREG(FCP_COL, 0);
	SETREG(FCP_ROW_L(0), format_mark_page_offset);
	SETREG(FCP_ROW_H(0), format_mark_page_offset);

	// At this point, we do not have to check Waiting Room status before issuing a command,
	// because scan list loading has been completed just before this function is called.
	SETREG(FCP_ISSUE, NULL);

	// wait for the FC_COL_ROW_READ_OUT command to be accepted by bank #0
	while ((GETREG(WR_STAT) & 0x00000001) != 0);

	// wait until bank #0 finishes the read operation
	while (BSP_FSM(0) != BANK_IDLE);

	// Now that the read operation is complete, we can check interrupt flags.
	temp = BSP_INTR(0) & FIRQ_ALL_FF;

	// clear interrupt flags
	CLR_BSP_INTR(0, 0xFF);

	if (temp != 0)
	{
		return FALSE;	// the page contains all-0xFF (the format mark does not exist.)
	}
	else
	{
		return TRUE;	// the page contains something other than 0xFF (it must be the format mark)
	}
}

// BSP interrupt service routine
void ftl_isr(void)
{
	UINT32 bank;
	UINT32 bsp_intr_flag;

	uart_print("BSP interrupt occured...");
	// interrupt pending clear (ICU)
	SETREG(APB_INT_STS, INTR_FLASH);

	for (bank = 0; bank < NUM_BANKS; bank++) {
		while (BSP_FSM(bank) != BANK_IDLE);
		// get interrupt flag from BSP
		bsp_intr_flag = BSP_INTR(bank);

		if (bsp_intr_flag == 0) {
			continue;
		}
		UINT32 fc = GETREG(BSP_CMD(bank));
		// BSP clear
		CLR_BSP_INTR(bank, bsp_intr_flag);

		// interrupt handling
		if (bsp_intr_flag & FIRQ_DATA_CORRUPT) {
			uart_printf("BSP interrupt at bank: 0x%x", bank);
			uart_print("FIRQ_DATA_CORRUPT occured...");
		}
		if (bsp_intr_flag & (FIRQ_BADBLK_H | FIRQ_BADBLK_L)) {
			uart_printf("BSP interrupt at bank: 0x%x", bank);
			if (fc == FC_COL_ROW_IN_PROG || fc == FC_IN_PROG || fc == FC_PROG) {
				uart_print("find runtime bad block when block program...");
			}
			else {
				uart_printf("find runtime bad block when block erase...vblock #: %d", GETREG(BSP_ROW_H(bank)) / PAGES_PER_BLK);
				ASSERT(fc == FC_ERASE);
			}
		}
	}
}

/////////////////////////////////////////////////////////////////////

static UINT32 set_valid_ppa(UINT32 const ppa)
{
	return VAL & ppa;
}

static UINT32 set_invalid_ppa(UINT32 const ppa)
{
	return INVAL | ppa;
}

static BOOL32 is_valid_ppa(UINT32 ppa)
{
	if(INVAL & ppa)
	{
		return FALSE;
	}
	else
		return TRUE;
}

/*lpa瑜?諛쏆븘???명? 留ㅽ븨???ㅼ졇?? * ?명? 留ㅽ븨???ㅽ봽?뗭쓣 諛섑솚
 * ?놁쑝硫??몃갭
 */
static UINT32 get_delta_map_offset(UINT32 const lpa)
{
	UINT32 offset = g_delta_pmt_pointer;
	for(; offset != g_delta_pmt_pointer + 1; offset = (offset - 1) % NUM_MAX_DELTA_PAGES)
	{
		if(lpa == get_delta_lpa(offset))//read_dram_32(DELTA_PMT_ADDR + sizeof(UINT32) * 2 * offset))
		{
			return offset;
		}
	}
	return INVAL;
}

//?뗭틦?쒕? ?덉벐寃??섎㈃??//merge?섎뒗 ?곹솴???명? 留ㅽ븨????李쇱쓣?뚮옉
//read ?????⑹튇 媛믪쓣 ?섍꺼二쇰뒗 ?곹솴 ?먭?吏??//lpa???명? ppa留??몄옄濡?二쇰㈃
//?섎㉧吏???ш린???뚯븘??//踰꾪띁???쇰떒 ?쒗봽踰꾪띁濡??좉쾶
//?섏쨷??肄붾뱶 ??吏꾪뻾?섏빞 ?쒗봽踰꾪띁 ?⑤룄 ?섎뒗吏 紐살벐?붿? ?????덉쓣寃?媛숈븘
static void merge(UINT32 const bank, UINT32 const lpa, UINT32 const ppa_delta, UINT32 const buf_ptr)
{
	ASSERT(ppa_delta != INVAL);
	UINT32 ppa_ori = set_valid_ppa(get_vpn(lpa));

	//?명????ㅻ━吏???쎌뼱??	read_from_delta(bank, lpa, ppa_delta, TEMP_BUF_PTR(1));
	nand_page_read(bank, get_pbn(ppa_ori), get_offset(ppa_ori), TEMP_BUF_PTR(0));

	//read_from_delta????

	//xor ?댁꽌 ?쒗봽踰꾪봽0?????	xor_buffer(TEMP_BUF_PTR(0), TEMP_BUF_PTR(1), buf_ptr);
}

static void read_from_delta(UINT32 const bank, UINT32 const lpa, UINT32 const delta_ppa, UINT32 const dst_buf)		//read delta to buf_addr
{
	UINT32 delta_read_start;			//pointer(start of delta)
	UINT32 pbn, offset;					//vbn of delta_ppa, offset of delta_ppa in vbn
	UINT32 buf_ptr;

	pbn = get_pbn(delta_ppa);
	offset = get_offset(delta_ppa);

	if(delta_ppa == NULL)
	{
		buf_ptr = DELTA_BUF(bank);
	}
	else
	{
		nand_page_read(bank, pbn, offset, DELTA_TEMP_BUF_PTR(0));	//load from nand to temp_buffer
		buf_ptr = DELTA_TEMP_BUF_PTR(0);
	}

	delta_read_start = find_delta_data(buf_ptr, lpa);		//find delta data in temp_buffer;
	_lzf_decompress(delta_read_start, dst_buf);

	return;
}

/*
* ?뺤텞?섎뒗 ?⑥닔
* compress
* ?몄옄 : 踰꾪띁 ?먭컻
* 由ы꽩 : TRUE/FALSE
* ?깃났?섎㈃ TEMP_BUF_PTR(2)???뺤텞??寃껋씠 ?ㅼ뼱媛?*/
//buf_data : ?먮옒 nand???덈뜕 ?섏씠吏, buf_write : write???덈줈 ?곗씠???섏씠吏
//BOOL32 compress(TEMP_BUF_PTR(0), WR_BUF_PTR(g_ftl_write_buf_id))
static BOOL32 compress(UINT32 buf_data, UINT32 buf_write)
{
	BOOL32 success;

	//buf_data, buf_write瑜??뺤텞?댁꽌 TMP_BUF_PTR(1)濡?蹂대궡以??
	//xor_buffer(UINT32 const src0, UINT32 const src1, UINT32 const dst);
	xor_buffer(buf_data, buf_write, TEMP_BUF_PTR(1));
	uart_printf("xor_buffer");

	//TEMP_BUF_PTR(1)?먯꽌 ?뺤텞?섏뿬 TEMP_BUF_PTR(2)濡?蹂대궡以??
	//success??_lzf_compress媛 ?깃났?곸쑝濡??섏뿀?붿?瑜??뚮젮以??
	success = (_lzf_compress(TEMP_BUF_PTR(1), TEMP_BUF_PTR(2)) > 0);

	uart_printf("lzf_compress : %d", success);

	return success;
}

static UINT32 get_next_delta_table_space(UINT32 const bank, UINT32 const lpa)
{
	UINT32 offset;
	UINT32 new_ppa = 0;
GET_NEXT_DELTA_TABLE_SPACE_START:

	offset = (g_delta_pmt_pointer + 1) % NUM_MAX_DELTA_PAGES;
	//g_delta_pmt_pointer = (g_delta_pmt_pointer + 1) % NUM_MAX_DELTA_PAGES;
	UINT32 old_delta_ppa = get_delta_ppa(offset);
	UINT32 old_delta_lpa = get_delta_lpa(offset);

	if(old_delta_lpa == lpa)
	{
		/*
		 * 吏湲?媛由ы궓 ?덉씠 ?먭린 ?먯떊???ㅼ뼱?덈뒗 ?뷀듃由?		 * 洹몃깷 ??뼱 ?
		 */
		goto GET_NEXT_DELTA_TABLE_SPACE_END;
	}
	else
	{
		/*
		 * ?먭린 ?뷀듃由ш? ?꾨떂
		 */
		if(old_delta_ppa == 0)
		{
			/*
			 * 吏湲?媛由ы궓 ?덉씠 ?섑븘 踰꾪띁???덈뒗?덉엫
			 * ?섎? 鍮쇨퀬 ?댁찈怨??섎㈃ 蹂듭옟??吏?덇퉴
			 * 洹몃깷 ?ㅼ쓬?먯꽌 李얜룄濡??섏옄
			 */
			g_delta_pmt_pointer = offset;
			goto GET_NEXT_DELTA_TABLE_SPACE_START;
		}
		else if(is_valid_ppa(old_delta_ppa) == FALSE)
		{
			/*
			 * 吏湲??ъ씤?곌? 媛由ы궎???뷀듃由ш? ?몃갭由щ뱶???덉씠硫?			 * 洹??꾩튂?먮떎 洹몃깷 ??뼱 ?곕㈃ ??			 */
			/*
			 * ?몃갭由щ뱶 ???덉씠?쇰뒗嫄?			 * 洹??명? 留ㅽ븨 ?뚯씠釉?移몄뿉 泥섏쓬 ?곕뒗嫄곕? ?섎?!
			 */
			goto GET_NEXT_DELTA_TABLE_SPACE_END;
		}

		else
		{
			/*
			 * 吏湲??ъ씤?곌? 媛由ы궎???뷀듃由ш? 諛몃━?쒗븯硫?			 * 洹몃냸??已볦븘蹂대궡?쇳븿
			 * merge?댁꽌
			 * ?ㅼ떆 ?
			 */
			//已볤꺼?섎뒗???⑥빞??			if(new_ppa == 0)
			{
				//?뺤긽?곸씤 寃쎈줈
				new_ppa = assign_new_write_vpn(bank);
			}

			if(offset != (g_delta_pmt_pointer + 1) % NUM_MAX_DELTA_PAGES)
			{
				/*
				 * 萸붽? 萸붽????섑빐??寃??꾨━?쇳뵾?먯씠瑜?媛붾떎 ?붾뜑??				 * delta 留ㅽ븨???ъ씤?곌? 諛붾뚯뼱 ?덉쓬
				 * ?꾨쭏??gc?섎㈃???명?媛 已볤꺼????				 * ?명? 留ㅽ븨??諛붽씔 寃쎌슦??				 * 吏湲덇퍘 ?섎뜕嫄?洹몃깷 ?λ릺怨?				 * 泥섏쓬遺???ㅼ떆 ?쒖옉!
				 */
				goto GET_NEXT_DELTA_TABLE_SPACE_START;
			}
			else
			{
				/*
				 * 湲곗〈???덈뜕??癒몄??댁꽌 已볦븘?댁옄~
				 */
				UINT32 vblock   = new_ppa / PAGES_PER_BLK;
				UINT32 page_num = new_ppa % PAGES_PER_BLK;
				set_lpn(bank, page_num, old_delta_lpa);
				set_vpn(old_delta_lpa, new_ppa);
				set_vcount(bank, vblock, get_vcount(bank, vblock) + 1);
				set_vcount(bank, old_delta_ppa / PAGES_PER_BLK, get_vcount(bank, old_delta_ppa / PAGES_PER_BLK) - 1);

				merge(bank, old_delta_lpa, old_delta_ppa, TEMP_BUF_PTR(0));
				nand_page_program(bank, get_pbn(new_ppa), get_offset(new_ppa), TEMP_BUF_PTR(0));
			}
		}
	}

GET_NEXT_DELTA_TABLE_SPACE_END:
	g_delta_pmt_pointer = offset;
	return g_delta_pmt_pointer;
}

static void write_to_delta(UINT32 const bank, UINT32 const lpa, UINT32 const buf_addr)	//write to delta write buffer
{
	UINT16 cs;
	UINT32 i;
	UINT32 data_cnt;
	UINT32 lpa_out, offset_out;
	UINT32 delta_ppn;
	UINT32 vblock, voffset;

	//?뺤텞???명????ъ씠利?李얠쓬
	cs = read_dram_16(buf_addr);

	//?명? 踰꾪띁媛 ?뺤텞???명?瑜??ｌ쓣 ???덉쑝硫?	if(is_remain_delta_buffer(bank, cs))
	{
		//?꾩옱 ??lpa媛 ?명? 踰꾪띁???덈뒗 lpa? 媛숈? 寃껋씠 ?덉쑝硫?		//?명? 踰꾪띁???덈뒗 lpa瑜?INVALID ?쒗궡.
		data_cnt = read_dram_32(DELTA_BUF(bank));
		for(i = 0; i < data_cnt; i++)
		{
			lpa_out = read_dram_32(buf_addr + (2 * i + 2) * sizeof(UINT32));
			if(lpa_out == lpa)
			{
				mem_set_dram(buf_addr + (2 * i + 2) * sizeof(UINT32), INVAL, sizeof(UINT32));
			}
		}

	}
	else	//?명? 踰꾪띁媛 ?뺤텞???명?瑜?紐??ｌ쓬
	{
		//???섏씠吏 ?좊떦
		delta_ppn = assign_new_write_vpn(bank);

		//?명? ?섏씠吏 ?댁쓽 lpn??????섏씠吏 留ㅽ븨 ?뚯씠釉붿쓽 delta_ppa?ㅼ쓣 諛붽퓭以?
		data_cnt = read_dram_32(DELTA_BUF(bank));
		for(i = 0; i < data_cnt; i++)
		{
			lpa_out = read_dram_32(DELTA_BUF(bank) + (2 * i + 2) * sizeof(UINT32));
			if(lpa_out != INVAL)
			{
				offset_out = get_delta_map_offset(lpa);
				set_delta_ppa(offset_out, delta_ppn);
			}
		}
		//nand page???!!
		vblock = get_pbn(delta_ppn);
		voffset = get_offset(delta_ppn);
		nand_page_program(bank, vblock, voffset, DELTA_BUF(bank));

		//LPA BUF?먮룄 ?⑥쨲!!
		set_lpn(bank, delta_ppn / PAGES_PER_BLK, INVAL);

		//?명? 硫뷀?, offset 珥덇린???댁＜湲?		next_delta_meta[bank] = DELTA_BUF(bank) + sizeof(UINT32);
		next_delta_offset[bank] = DELTA_BUF(bank) + (1 + 2 * MAX_DELTAS_PER_PAGE) * sizeof(UINT32);

	}

	//delta?댁뿉 delta?⑥＜怨? lpn ???⑥＜怨? offset???⑥＜怨?	//?꾩뿉??怨듯넻?섎뒗 遺遺꾩씠??類?	write_dram_32(next_delta_meta[bank], next_delta_offset[bank]);
	write_dram_32(next_delta_meta[bank] + sizeof(UINT32), lpa);
	mem_copy(next_delta_offset[bank], buf_addr, cs);
	next_delta_meta[bank] = next_delta_meta[bank] + 2 * sizeof(UINT32);
	next_delta_offset[bank] = next_delta_offset[bank] + (cs + sizeof(UINT32) - 1)/ sizeof(UINT32) * sizeof(UINT32);

}

static void xor_buffer(UINT32 const src0, UINT32 const src1, UINT32 const dst)
{
	UINT32 i;
	UINT32 temp0, temp1;

	for(i = 0; i < BYTES_PER_PAGE; i = i + sizeof(UINT32))
	{
		temp0 = read_dram_32(src0 + i);
		temp1 = read_dram_32(src1 + i);
		temp0 = temp0 ^ temp1;
		write_dram_32(dst + i, temp0);
	}
}

static UINT32 find_delta_data(UINT32 const buf_ptr, UINT32 const lpa)		//find delta data in temp_buffer;
{
	UINT32 i;

	//buf_ptr = buf_ptr + sizeof(UINT32);			//lpa starts at buf_ptr + 32
	UINT32 offset = buf_ptr + sizeof(UINT32);
	UINT32 cnt = read_dram_32(buf_ptr);

	for(i = 0; i < cnt; i++)
	{
		if(lpa == read_dram_32(offset + sizeof(UINT32)))
			break;
		offset = offset + sizeof(UINT32)*2;
	}

	if(i == cnt)
	{
		return -1;
	}
	else
	{
		return read_dram_32(offset);
	}
}

static void _lzf_decompress (UINT32 const in_data_base, UINT32 const out_data_base)        //decompress data
{
	UINT16 cs;
	UINT16 cs_cluster;
	volatile UINT16 header1[CLUSTER_SIZE];		//compressed buffer
	volatile UINT16 header2[CLUSTER_SIZE];		//decompressed buffer
	UINT32 i;
	UINT32 in_data = in_data_base + sizeof(UINT32);
	UINT32 out_data = out_data_base;

	cs = read_dram_16(in_data_base);

	for(i = 0; i < NUM_COMP_CLUSTER; i++)
	{
		//load meta & data
		cs_cluster = read_dram_16(in_data);		//?먮즺?ш린 ?쎌쓬
		mem_copy(header1, in_data + sizeof(UINT32), ALIGN32(cs_cluster));	// copy compressed data to header1(SRAM)

		ASSERT (lzf_decompress ((void*)header1, cs_cluster, (void*)header2, CLUSTER_SIZE) == CLUSTER_SIZE);

		mem_copy(out_data, header2, CLUSTER_SIZE);		// copy decompressed data to out_data(DRAM)

		//next cluster
		in_data = in_data + ALIGN32(cs_cluster) + sizeof(UINT32);
		out_data = out_data + CLUSTER_SIZE;
	}
}

static UINT32 _lzf_compress (UINT32 const in_data_base, UINT32 const out_data_base)
{
	UINT16 cs = 0;
	UINT16 cs_cluster;
	volatile UINT16 header1[CLUSTER_SIZE + 1];    //decompressed buffer
	volatile UINT16 header2[CLUSTER_SIZE + 1];    //compressed buffer
	UINT32 i;
	UINT32 in_data = in_data_base;
	UINT32 out_data = out_data_base + sizeof(UINT32);	//front of out_data_base : cs

	for(i = 0; i < NUM_COMP_CLUSTER; i++)
	{
		mem_copy(header1, in_data, CLUSTER_SIZE);

		cs_cluster = lzf_compress ((void*)header1, CLUSTER_SIZE, (void*)&header2[1], CLUSTER_SIZE - 4);	//compressed size of one cluster

		//memory check
		cs = cs + ALIGN32(cs_cluster) + sizeof(UINT32);			//cs = cs + compressed data length + header
		if(cs >= MAX_COMPRESS_SIZE)
		{
			return 0;											//write uncompressed
		}

		//write meta & data
		write_dram_16(out_data, cs_cluster);
		mem_copy(out_data + sizeof(UINT32), header2, ALIGN32(cs_cluster));

		//next cluster
		in_data = in_data + CLUSTER_SIZE;
		out_data = out_data + ALIGN32(cs_cluster) + sizeof(UINT32);

	}

	write_dram_16(out_data_base, cs);		//write cs

	return cs;
}

static UINT32 is_remain_delta_buffer(UINT32 const bank, UINT32 const cs)	//is remain in delta_buffer?
{
	if((next_delta_offset[bank] - DELTA_BUF(bank) + cs) > BYTES_PER_PAGE)
	{
		return 0;
	}
	if(next_delta_meta[bank] / (2 * sizeof(UINT32)) == MAX_DELTAS_PER_PAGE)
	{
		return 0;
	}

	return 1;
}
