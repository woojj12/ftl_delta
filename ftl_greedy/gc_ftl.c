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

//----------------------------------
// macro
//----------------------------------

#define CMT_SIZE  		256
#define MAPBLKS_PER_BANK	2//((GTD_SIZE_PER_BANK + PAGES_PER_BLK - 1) / PAGES_PER_BLK)
#define MAPPINGS_PER_PAGE	(BYTES_PER_PAGE / sizeof(UINT32))

#define VC_MAX              0xCDCD
#define MISCBLK_VBN         0x1 // vblock #1 <- misc metadata
#define META_BLKS_PER_BANK  (1 +  MAPBLKS_PER_BANK) // include block #0,

// the number of sectors of misc. metadata info.
#define NUM_MISC_META_SECT  ((sizeof(misc_metadata) + BYTES_PER_SECTOR - 1)/ BYTES_PER_SECTOR)
#define NUM_VCOUNT_SECT     ((VBLKS_PER_BANK * sizeof(UINT16) + BYTES_PER_SECTOR - 1) / BYTES_PER_SECTOR)

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
    UINT32 cur_map_write_vpn; // physical page for new write
//    UINT32 cur_miscblk_vpn; // current write vpn for logging the misc. metadata
//    UINT32 cur_mapblk_vpn[MAPBLKS_PER_BANK]; // current write vpn for logging the age mapping info.
    UINT32 gc_vblock; // vblock number for garbage collection
    UINT32 free_blk_cnt; // total number of free block count
    UINT32 lpn_list_of_cur_vblock[PAGES_PER_BLK]; // logging lpn list of current write vblock for GC
}misc_metadata; // per bank

//----------------------------------
// FTL metadata (maintain in SRAM)
//----------------------------------
static misc_metadata  g_misc_meta[NUM_BANKS];
//static ftl_statistics g_ftl_statistics[NUM_BANKS];
static UINT32		  g_bad_blk_count[NUM_BANKS];

// SATA read/write buffer pointer id
UINT32 				  g_ftl_read_buf_id;
UINT32 				  g_ftl_write_buf_id;

typedef struct cmt
{
	UINT32 lpn;
	UINT32 vpn;
	BOOL32 sc;
} CMT;

CMT cmt[CMT_SIZE];
UINT32 cmt_hand;

UINT32 flag;

#define SET_WRITE	(flag = flag | 0x80000000)
#define CLEAR_WRITE	(flag = flag & 0x7FFFFFFF)
#define SET_READ	(flag = flag | 0x40000000)
#define CLEAR_READ	(flag = flag & 0xb0000000)
#define IS_WRITE	(flag & 0x80000000)
#define IS_READ		(flag & 0x40000000)

#define SET_DIRTY(vpn)	(vpn | 0x80000000)
#define SET_CLEAN(vpn)	(vpn & 0x7FFFFFFF)
#define IS_CLEAN(vpn)	(!(vpn & 0x80000000))

UINT32 gtd[NUM_BANKS][GTD_SIZE_PER_BANK];

UINT32 map_blk[NUM_BANKS][2];


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
//#define set_miscblk_vpn(bank, vpn)    (g_misc_meta[bank].cur_miscblk_vpn = vpn)
#define get_mapblk_vpn(bank, mapblk_lbn)      (g_misc_meta[bank].cur_mapblk_vpn[mapblk_lbn])
//#define set_mapblk_vpn(bank, mapblk_lbn, vpn) (g_misc_meta[bank].cur_mapblk_vpn[mapblk_lbn] = vpn)
#define CHECK_LPAGE(lpn)              ASSERT((lpn) < NUM_LPAGES)
#define CHECK_VPAGE(vpn)              ASSERT((vpn) < (VBLKS_PER_BANK * PAGES_PER_BLK))

#define get_cur_map_write_vpn(bank)       (g_misc_meta[bank].cur_map_write_vpn)
#define set_new_map_write_vpn(bank, vpn)  (g_misc_meta[bank].cur_map_write_vpn = vpn)

//----------------------------------
// FTL internal function prototype
//----------------------------------
static void   format(void);
static void   write_format_mark(void);
static void   sanity_check(void);
static void   init_metadata_sram(void);
static void   load_metadata(void);
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

//////////////////
static void load_pmt(UINT32 const bank);
static UINT32 gc_get_vpn(UINT32 const lpn);
static void gc_set_vpn(UINT32 const lpn, UINT32 const vpn);
static void save_pmt(UINT32 bank);

static UINT32 assign_new_map_write_vpn(UINT32 const bank);

static void evict_mapping(void);
static UINT32 assign_new_map_write_vpn(UINT32 const bank);

static void sanity_check(void)
{
    UINT32 dram_requirement = RD_BUF_BYTES + WR_BUF_BYTES + COPY_BUF_BYTES + FTL_BUF_BYTES
        + HIL_BUF_BYTES + TEMP_BUF_BYTES + BAD_BLK_BMP_BYTES + VCOUNT_BYTES;

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

UINT32 ftl_r;
UINT32 ftl_w;
UINT32 write_p;

UINT32 data_prog, data_read;
UINT32 set_v, get_v;
UINT32 cmt_w_hit, cmt_r_hit;
UINT32 map_prog, map_read;
UINT32 gc;
UINT32 get_v_w = 0;
UINT32 get_v_w_hit = 0;

UINT32 gc_prog;
UINT32 misc_w;
UINT32 erase;
UINT32 load_pm, save_pm;
UINT32 map_chg_prog;

void ftl_flush(void)
{
    /* ptimer_start(); */
//    logging_pmap_table();
////    logging_misc_metadata();
//	return;
    /* ptimer_stop_and_uart_print(); */
	uart_printf("ftl_read	%d", ftl_r);
	uart_printf("ftl_write	%d", ftl_w);
	uart_printf("write_page	%d", write_p);
	uart_printf("data_program	%d", data_prog);
	uart_printf("data_read	%d", data_read);
	uart_printf("garbage_collection	%d", gc);
	uart_printf("set_vpn	%d", set_v);
	uart_printf("get_vpn	%d", get_v);
	uart_printf("cmt_write_hit	%d", cmt_w_hit);
	uart_printf("cmt_read_hit	%d", cmt_r_hit);
	uart_printf("mapping_program	%d", map_prog);
	uart_printf("mapping_read	%d", map_read);
	uart_printf("erase	%d", erase);
	uart_printf("load_page_map	%d",load_pm);
	uart_printf("save_page_map	%d", save_pm);
	uart_printf("map_block_change	%d", map_chg_prog);
	uart_printf("get_v_w	%d", get_v_w);
	uart_printf("get_v_w_hit	%d", get_v_w_hit);
}

void ftl_read(UINT32 const lba, UINT32 const num_sectors)
{
	SET_READ;

    UINT32 remain_sects, num_sectors_to_read;
    UINT32 lpn, sect_offset;
    UINT32 bank, vpn;

    ftl_r++;

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
        	data_read++;
            nand_page_ptread_to_host(bank,
                                     vpn / PAGES_PER_BLK,
                                     vpn % PAGES_PER_BLK,
                                     sect_offset,
                                     num_sectors_to_read);
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

    CLEAR_READ;
}

//************************//
//**** DFTL Algorithm ****//
//************************//
//Input: Request’s Logical Page Number (requestlpn), Request’s Size (requestsize)
//Output: NULL
//
//while requestsize = 0 do
//	if requestlpn miss in Cached Mapping Table then
//		if Cached Mapping Table is full then
//			/* Select entry for eviction using segmented LRU replacement algorithm */
//			victimlpn ←select victim entry()
//			if victimlast mod time = victimload time then
//				/*victimtype : Translation or Data Block
//				Translation Pagevictim : Physical
//				Translation-Page Number containing victim entry */
//				Translation Pagevictim ←consult GTD
//				(victimlpn)
//				victimtype ←Translation Block
//				DFTL Service Request(victim)
//			end
//			erase entry(victimlpn)
//		end
//		Translation Pagerequest ←
//		consult GTD(requestlpn)
//		/* Load map entry of the request from flash into CachedMapping Table */
//		load entry(Translation Pagerequest )
//	end
//	requesttype ←Data Block
//	requestppn ←CMT lookup(requestlpn)
//	DFTL Service Request(request)
//	requestsize- -
//end

void ftl_write(UINT32 const lba, UINT32 const num_sectors)
{
	SET_WRITE;

	ftl_w++;

    UINT32 remain_sects, num_sectors_to_write;
    UINT32 lpn, sect_offset;

    lpn          = lba / SECTORS_PER_PAGE;
    sect_offset  = lba % SECTORS_PER_PAGE;
    remain_sects = num_sectors;

    //while requestsize = 0 do
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

    CLEAR_WRITE;
}

static void write_page(UINT32 const lpn, UINT32 const sect_offset, UINT32 const num_sectors)
{
	write_p++;
	CHECK_LPAGE(lpn);
    ASSERT(sect_offset < SECTORS_PER_PAGE);
    ASSERT(num_sectors > 0 && num_sectors <= SECTORS_PER_PAGE);

    UINT32 bank, old_vpn, new_vpn;
    UINT32 vblock, page_num, page_offset, column_cnt;

    bank        = get_num_bank(lpn); // page striping
    page_offset = sect_offset;
    column_cnt  = num_sectors;

    /*
     * Parial Write라면 매핑을 읽어오고 페이지를 읽어와서 홀을 채움
     * Full Page Write라면 매핑을 읽어올 필요 없음
     */

//    g_ftl_statistics[bank].page_wcount++;
    if (num_sectors != SECTORS_PER_PAGE)
    {
        old_vpn  = get_vpn(lpn);
        CHECK_VPAGE (old_vpn);
        if (old_vpn != NULL)
        {
            vblock   = old_vpn / PAGES_PER_BLK;
            page_num = old_vpn % PAGES_PER_BLK;
            if ((num_sectors <= 8) && (page_offset != 0))
            {
                // one page async read
                data_read++;
                nand_page_read(bank,
                               vblock,
                               page_num,
                               FTL_BUF(bank));
                // copy `left hole sectors' into SATA write buffer
                if (page_offset != 0)
                {
                    mem_copy(WR_BUF_PTR(g_ftl_write_buf_id),
                             FTL_BUF(bank),
                             page_offset * BYTES_PER_SECTOR);
                }
                // copy `right hole sectors' into SATA write buffer
                if ((page_offset + column_cnt) < SECTORS_PER_PAGE)
                {
                    UINT32 const rhole_base = (page_offset + column_cnt) * BYTES_PER_SECTOR;

                    mem_copy(WR_BUF_PTR(g_ftl_write_buf_id) + rhole_base,
                             FTL_BUF(bank) + rhole_base,
                             BYTES_PER_PAGE - rhole_base);
                }
            }
            // left/right hole async read operation (two partial page read)
            else
            {
                // read `left hole sectors'
                if (page_offset != 0)
                {
                    data_read++;
                    nand_page_ptread(bank,
                                     vblock,
                                     page_num,
                                     0,
                                     page_offset,
                                     WR_BUF_PTR(g_ftl_write_buf_id),
                                     RETURN_WHEN_DONE);
                }
                // read `right hole sectors'
                if ((page_offset + column_cnt) < SECTORS_PER_PAGE)
                {
                    data_read++;
                    nand_page_ptread(bank,
                                     vblock,
                                     page_num,
                                     page_offset + column_cnt,
                                     SECTORS_PER_PAGE - (page_offset + column_cnt),
                                     WR_BUF_PTR(g_ftl_write_buf_id),
                                     RETURN_WHEN_DONE);
                }
            }
            // invalid old page (decrease vcount)
            set_vcount(bank, vblock, get_vcount(bank, vblock) - 1);
        }
        else
        {
    		if(page_offset != 0)
    			mem_set_dram(WR_BUF_PTR(g_ftl_write_buf_id),
    					0,
    					page_offset * BYTES_PER_SECTOR);
    		if((page_offset + num_sectors) < SECTORS_PER_PAGE)
    		{
    			UINT32 const rhole_base = (page_offset + num_sectors) * BYTES_PER_SECTOR;
    			mem_set_dram(WR_BUF_PTR(g_ftl_write_buf_id) + rhole_base, 0, BYTES_PER_PAGE - rhole_base);
    		}
        }
    }

    new_vpn  = assign_new_write_vpn(bank);
    CHECK_VPAGE (new_vpn);

    vblock   = new_vpn / PAGES_PER_BLK;
    page_num = new_vpn % PAGES_PER_BLK;
    ASSERT(get_vcount(bank,vblock) < (PAGES_PER_BLK - 1));

    // write new data (make sure that the new data is ready in the write buffer frame)
    // (c.f FO_B_SATA_W flag in flash.h)
    data_prog++;
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
	if(IS_WRITE)
	{
		get_v_w++;
	}
	else
	{
		get_v++;
	}
    CHECK_LPAGE(lpn);
//    return read_dram_32(PAGE_MAP_ADDR + lpn * sizeof(UINT32));
    UINT32 index;
    for(index = 0; index < CMT_SIZE; index++)
    {
    	if(cmt[index].lpn == lpn)
    	{
    		if(IS_WRITE)
    			get_v_w_hit++;
    		else
    			cmt_r_hit++;
    		return SET_CLEAN(cmt[index].vpn);
    	}
    }
    /*
     * not in CMT
     * now select an victim
     */
    /*
     * now, cmt[cmt_hand] is a victim
     */
    evict_mapping();
    /*
     * now get the mapping info
     */
    UINT32 gtd_index;// = lpn / (MAPPINGS_PER_PAGE*NUM_BANKS);
    UINT32 mapping_bank = get_num_bank(lpn);

    UINT32 offset_in_bank = lpn / NUM_BANKS;
    UINT32 offset_in_page = offset_in_bank % MAPPINGS_PER_PAGE;
    gtd_index = offset_in_bank / MAPPINGS_PER_PAGE;
    UINT32 mapping_vpn = gtd[mapping_bank][gtd_index];

    if(mapping_vpn == INVALID)
    {
    	return NULL;
    }

    map_read++;
    nand_page_read(mapping_bank,
    		mapping_vpn / PAGES_PER_BLK,
    		mapping_vpn % PAGES_PER_BLK,
    		TEMP_BUF_ADDR);
    cmt[cmt_hand].lpn = lpn;
    cmt[cmt_hand].vpn = read_dram_32(TEMP_BUF_ADDR + sizeof(UINT32) * offset_in_page);
    cmt[cmt_hand].sc = TRUE;

    UINT32 ret = SET_CLEAN(cmt[cmt_hand].vpn);
    cmt_hand = (cmt_hand + 1) % CMT_SIZE;
    return ret;
}

static void evict_mapping(void)
{
	if(cmt[cmt_hand].lpn == INVALID)
		return;
    while(1)
    {
    	if(cmt[cmt_hand].sc == TRUE)
    	{
    		cmt[cmt_hand].sc = FALSE;
    		cmt_hand = (cmt_hand + 1) % CMT_SIZE;
    	}
    	else
    		break;
    }

    UINT32 gtd_index;
    UINT32 victim_lpn, victim_vpn;
    UINT32 mapping_vpn;
    UINT32 mapping_bank;
    victim_vpn = cmt[cmt_hand].vpn;

	/*
	 * dirty : 같은 translation page 에 속하는 dirty를
	 * 같이 업데이트 해 준다
	 * clean : 그냥 버린다
	 */

    if(IS_CLEAN(victim_vpn))
    {
    	//Clean
    	return;
    }

    //Dirty
    victim_lpn = cmt[cmt_hand].lpn;

    gtd_index = victim_lpn / (MAPPINGS_PER_PAGE*NUM_BANKS);
    mapping_bank = get_num_bank(victim_lpn);
    mapping_vpn = gtd[mapping_bank][gtd_index];

    if(mapping_vpn != INVALID)
    {
    	map_read++;

		nand_page_read(mapping_bank,
				mapping_vpn / PAGES_PER_BLK,
				mapping_vpn % PAGES_PER_BLK,
				FTL_BUF(mapping_bank));
    }
    else
    {
    	mem_set_dram(FTL_BUF(mapping_bank), 0, BYTES_PER_PAGE);
    }

    int index;
    for(index = 0; index < CMT_SIZE; index++)
	{
		if(get_num_bank(cmt[index].lpn) == mapping_bank)
		{
			if((!IS_CLEAN(cmt[index].vpn)) && \
					((cmt[index].lpn / (MAPPINGS_PER_PAGE*NUM_BANKS)) == gtd_index))
			{
				cmt[index].vpn = SET_CLEAN(cmt[index].vpn);
				write_dram_32(FTL_BUF(mapping_bank) + \
						sizeof(UINT32 ) * ((cmt[index].lpn/NUM_BANKS) % MAPPINGS_PER_PAGE),
						cmt[index].vpn);
			}
		}
	}

    mapping_vpn = assign_new_map_write_vpn(mapping_bank);

    gtd[mapping_bank][gtd_index] = mapping_vpn;

    map_prog++;
    nand_page_program(mapping_bank,
    		mapping_vpn / PAGES_PER_BLK,
    		mapping_vpn % PAGES_PER_BLK,
    		FTL_BUF(mapping_bank));
}

// set vpn to PAGE_MAP
static void set_vpn(UINT32 const lpn, UINT32 const vpn)
{
	set_v++;
    CHECK_LPAGE(lpn);
    ASSERT(vpn >= (META_BLKS_PER_BANK * PAGES_PER_BLK) && vpn < (VBLKS_PER_BANK * PAGES_PER_BLK));

//    write_dram_32(PAGE_MAP_ADDR + lpn * sizeof(UINT32), vpn);
    UINT32 index;
    for(index = 0; index < CMT_SIZE; index++)
    {
    	if(cmt[index].lpn == lpn)
    	{
    		cmt_w_hit++;
    		goto WRITE_CACHE_ENTRY;
    	}
    }
    /*
     * not in CMT
     */
    evict_mapping();

    index = cmt_hand;
    cmt_hand = (cmt_hand + 1) % CMT_SIZE;
    cmt[index].lpn = lpn;

    WRITE_CACHE_ENTRY:

    cmt[index].vpn = SET_DIRTY(vpn);
    cmt[index].sc = TRUE;
    return;
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

    //uart_printf("assign new vpn at bank %d vblock 0x%x", bank, vblock);

    // NOTE: if next new write page's offset is
    // the last page offset of vblock (i.e. PAGES_PER_BLK - 1),
    if ((write_vpn % PAGES_PER_BLK) == (PAGES_PER_BLK - 2))
    {
    	//uart_printf("assign new vblock 0x%x at bank %d",vblock, bank);
        // then, because of the flash controller limitation
        // (prohibit accessing a spare area (i.e. OOB)),
        // thus, we persistenly write a lpn list into last page of vblock.
        mem_copy(TEMP_BUF_ADDR, g_misc_meta[bank].lpn_list_of_cur_vblock, sizeof(UINT32) * PAGES_PER_BLK);
        // fix minor bug
        misc_w++;
        nand_page_ptprogram(bank, vblock, PAGES_PER_BLK - 1, 0,
                            ((sizeof(UINT32) * PAGES_PER_BLK + BYTES_PER_SECTOR - 1 ) / BYTES_PER_SECTOR), TEMP_BUF_ADDR);

        mem_set_sram(g_misc_meta[bank].lpn_list_of_cur_vblock, 0x00000000, sizeof(UINT32) * PAGES_PER_BLK);

        inc_full_blk_cnt(bank);

        // do garbage collection if necessary
        if (is_full_all_blks(bank))
        {
        	GC:
            garbage_collection(bank);
            return get_cur_write_vpn(bank);
        }
        do
        {
            vblock++;


            if(vblock == VBLKS_PER_BANK)
            {
            	goto GC;
            }
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

UINT32 gc;

static void garbage_collection(UINT32 const bank)
{
	gc++;
    ASSERT(bank < NUM_BANKS);
//    g_ftl_statistics[bank].gc_cnt++;

    UINT32 src_lpn;
    UINT32 vt_vblock;
    UINT32 free_vpn;
    UINT32 vcount; // valid page count in victim block
    UINT32 src_page;
    UINT32 gc_vblock;

//    g_ftl_statistics[bank].gc_cnt++;

    vt_vblock = get_vt_vblock(bank);   // get victim block
    vcount    = get_vcount(bank, vt_vblock);
    gc_vblock = get_gc_vblock(bank);
    free_vpn  = gc_vblock * PAGES_PER_BLK;

    uart_printf("garbage_collection bank %d, vblock %d",bank, vt_vblock);

    ASSERT(vt_vblock != gc_vblock);
    ASSERT(vt_vblock >= META_BLKS_PER_BANK && vt_vblock < VBLKS_PER_BANK);
    ASSERT(vcount < (PAGES_PER_BLK - 1));
    ASSERT(get_vcount(bank, gc_vblock) == VC_MAX);
    ASSERT(!is_bad_block(bank, gc_vblock));

    load_pmt(bank);

    // 1. load p2l list from last page offset of victim block (4B x PAGES_PER_BLK)
    // fix minor bug
    misc_w++;
    nand_page_ptread(bank, vt_vblock, PAGES_PER_BLK - 1, 0,
                     ((sizeof(UINT32) * PAGES_PER_BLK + BYTES_PER_SECTOR - 1 ) / BYTES_PER_SECTOR), FTL_BUF(bank), RETURN_WHEN_DONE);
    mem_copy(g_misc_meta[bank].lpn_list_of_cur_vblock, FTL_BUF(bank), sizeof(UINT32) * PAGES_PER_BLK);
    // 2. copy-back all valid pages to free space
    for (src_page = 0; src_page < (PAGES_PER_BLK - 1); src_page++)
    {
        // get lpn of victim block from a read lpn list
        src_lpn = get_lpn(bank, src_page);
        CHECK_VPAGE(gc_get_vpn(src_lpn));

        // determine whether the page is valid or not
        if (gc_get_vpn(src_lpn) !=
            ((vt_vblock * PAGES_PER_BLK) + src_page))
        {
            // invalid page
            continue;
        }
        ASSERT(get_lpn(bank, src_page) != INVALID);
        CHECK_LPAGE(src_lpn);
        // if the page is valid,
        // then do copy-back op. to free space
        gc_prog++;
        nand_page_copyback(bank,
                           vt_vblock,
                           src_page,
                           free_vpn / PAGES_PER_BLK,
                           free_vpn % PAGES_PER_BLK);
        ASSERT((free_vpn / PAGES_PER_BLK) == gc_vblock);
        // update metadata
        gc_set_vpn(src_lpn, free_vpn);
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
    erase++;
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
    save_pmt(bank);
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
//    mem_set_dram(PAGE_MAP_ADDR, NULL, PAGE_MAP_BYTES);
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
//    logging_pmap_table();
//    logging_misc_metadata();

    write_format_mark();
	led(1);
    uart_print("format complete");
}
static void init_metadata_sram(void)
{
    UINT32 bank;
    UINT32 vblock;
    UINT32 mapblk_lbn;

    UINT32 index;
    for(index = 0; index < CMT_SIZE; index++)
    {
    	cmt[index].lpn = INVALID;
    	cmt[index].sc = FALSE;
    }

    //----------------------------------------
    // initialize misc. metadata
    //----------------------------------------
    for (bank = 0; bank < NUM_BANKS; bank++)
    {
    	for(index = 0; index < GTD_SIZE_PER_BANK; index++)
    	{
    		gtd[bank][index] = INVALID;
    	}
    	uart_printf("bank %d bad blk cnt %d", bank, get_bad_blk_cnt(bank));
        g_misc_meta[bank].free_blk_cnt = VBLKS_PER_BANK - META_BLKS_PER_BANK;
        g_misc_meta[bank].free_blk_cnt -= get_bad_blk_cnt(bank);
        // NOTE: vblock #0,1 don't use for user space
        write_dram_16(VCOUNT_ADDR + ((bank * VBLKS_PER_BANK) + 0) * sizeof(UINT16), VC_MAX);
        write_dram_16(VCOUNT_ADDR + ((bank * VBLKS_PER_BANK) + 1) * sizeof(UINT16), VC_MAX);
        write_dram_16(VCOUNT_ADDR + ((bank * VBLKS_PER_BANK) + 2) * sizeof(UINT16), VC_MAX);

        //----------------------------------------
        // assign misc. block
        //----------------------------------------
        // assumption: vblock #1 = fixed location.
        // Thus if vblock #1 is a bad block, it should be allocate another block.
//        set_miscblk_vpn(bank, MISCBLK_VBN * PAGES_PER_BLK - 1);
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
            	//uart_printf("bank %d set map blk 0x%x",bank, vblock);
//                set_mapblk_vpn(bank, mapblk_lbn, vblock * PAGES_PER_BLK);
            	map_blk[bank][mapblk_lbn] = vblock;
                write_dram_16(VCOUNT_ADDR + ((bank * VBLKS_PER_BANK) + vblock) * sizeof(UINT16), VC_MAX);
                mapblk_lbn++;
            }
        }
        set_new_map_write_vpn(bank, map_blk[bank][0] * PAGES_PER_BLK);
        //uart_printf("assigned map vpn at bank %d 0x%x", bank, map_blk[bank][0] * PAGES_PER_BLK);
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
            // 현재 next vblock부터 새로운 데이터를 저장을 시작
            set_new_write_vpn(bank, vblock * PAGES_PER_BLK);
            ASSERT(vblock < VBLKS_PER_BANK);
        }while(is_bad_block(bank, vblock) == TRUE);
    }
}
// load flushed FTL metadta
static void load_metadata(void)
{
    return;
}
static void write_format_mark(void)
{
//	// This function writes a format mark to a page at (bank #0, block #0).
//
//	#ifdef __GNUC__
//	extern UINT32 size_of_firmware_image;
//	UINT32 firmware_image_pages = (((UINT32) (&size_of_firmware_image)) + BYTES_PER_FW_PAGE - 1) / BYTES_PER_FW_PAGE;
//	#else
//	extern UINT32 Image$$ER_CODE$$RO$$Length;
//	extern UINT32 Image$$ER_RW$$RW$$Length;
//	UINT32 firmware_image_bytes = ((UINT32) &Image$$ER_CODE$$RO$$Length) + ((UINT32) &Image$$ER_RW$$RW$$Length);
//	UINT32 firmware_image_pages = (firmware_image_bytes + BYTES_PER_FW_PAGE - 1) / BYTES_PER_FW_PAGE;
//	#endif
//
//	UINT32 format_mark_page_offset = FW_PAGE_OFFSET + firmware_image_pages;
//
//	mem_set_dram(FTL_BUF_ADDR, 0, BYTES_PER_SECTOR);
//
//	SETREG(FCP_CMD, FC_COL_ROW_IN_PROG);
//	SETREG(FCP_BANK, REAL_BANK(0));
//	SETREG(FCP_OPTION, FO_E | FO_B_W_DRDY);
//	SETREG(FCP_DMA_ADDR, FTL_BUF_ADDR); 	// DRAM -> flash
//	SETREG(FCP_DMA_CNT, BYTES_PER_SECTOR);
//	SETREG(FCP_COL, 0);
//	SETREG(FCP_ROW_L(0), format_mark_page_offset);
//	SETREG(FCP_ROW_H(0), format_mark_page_offset);
//
//	// At this point, we do not have to check Waiting Room status before issuing a command,
//	// because we have waited for all the banks to become idle before returning from format().
//	SETREG(FCP_ISSUE, NULL);
//
//	// wait for the FC_COL_ROW_IN_PROG command to be accepted by bank #0
//	while ((GETREG(WR_STAT) & 0x00000001) != 0);
//
//	// wait until bank #0 finishes the write operation
//	while (BSP_FSM(0) != BANK_IDLE);
	return;
}
static BOOL32 check_format_mark(void)
{
//	// This function reads a flash page from (bank #0, block #0) in order to check whether the SSD is formatted or not.
//
//	#ifdef __GNUC__
//	extern UINT32 size_of_firmware_image;
//	UINT32 firmware_image_pages = (((UINT32) (&size_of_firmware_image)) + BYTES_PER_FW_PAGE - 1) / BYTES_PER_FW_PAGE;
//	#else
//	extern UINT32 Image$$ER_CODE$$RO$$Length;
//	extern UINT32 Image$$ER_RW$$RW$$Length;
//	UINT32 firmware_image_bytes = ((UINT32) &Image$$ER_CODE$$RO$$Length) + ((UINT32) &Image$$ER_RW$$RW$$Length);
//	UINT32 firmware_image_pages = (firmware_image_bytes + BYTES_PER_FW_PAGE - 1) / BYTES_PER_FW_PAGE;
//	#endif
//
//	UINT32 format_mark_page_offset = FW_PAGE_OFFSET + firmware_image_pages;
//	UINT32 temp;
//
//	flash_clear_irq();	// clear any flash interrupt flags that might have been set
//
//	SETREG(FCP_CMD, FC_COL_ROW_READ_OUT);
//	SETREG(FCP_BANK, REAL_BANK(0));
//	SETREG(FCP_OPTION, FO_E);
//	SETREG(FCP_DMA_ADDR, FTL_BUF_ADDR); 	// flash -> DRAM
//	SETREG(FCP_DMA_CNT, BYTES_PER_SECTOR);
//	SETREG(FCP_COL, 0);
//	SETREG(FCP_ROW_L(0), format_mark_page_offset);
//	SETREG(FCP_ROW_H(0), format_mark_page_offset);
//
//	// At this point, we do not have to check Waiting Room status before issuing a command,
//	// because scan list loading has been completed just before this function is called.
//	SETREG(FCP_ISSUE, NULL);
//
//	// wait for the FC_COL_ROW_READ_OUT command to be accepted by bank #0
//	while ((GETREG(WR_STAT) & 0x00000001) != 0);
//
//	// wait until bank #0 finishes the read operation
//	while (BSP_FSM(0) != BANK_IDLE);
//
//	// Now that the read operation is complete, we can check interrupt flags.
//	temp = BSP_INTR(0) & FIRQ_ALL_FF;
//
//	// clear interrupt flags
//	CLR_BSP_INTR(0, 0xFF);
//
//	if (temp != 0)
//	{
//		return FALSE;	// the page contains all-0xFF (the format mark does not exist.)
//	}
//	else
//	{
//		return TRUE;	// the page contains something other than 0xFF (it must be the format mark)
//	}
	return FALSE;
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

static void load_pmt(UINT32 const bank)
{
	UINT32 i;
	UINT32 mapping_vpn;
	/*
	 * for gc,
	 * laod page mappings to dram
	 * so set_vpn, get_vpn can skip nand program while gc
	 */
	for(i=0; i<GTD_SIZE_PER_BANK; i++)
	{
		mapping_vpn = gtd[bank][i];
		if(mapping_vpn == INVALID)
		{
			mem_set_dram(GC_BUF(i), 0, BYTES_PER_PAGE);
		}
		else
		{
			load_pm++;
			nand_page_read(bank,
					mapping_vpn / PAGES_PER_BLK,
					mapping_vpn % PAGES_PER_BLK,
					GC_BUF(i));
		}
	}
	/*
	 * flush cache-cmt to dram
	 */
	UINT32 offset;
	for(i=0; i<CMT_SIZE; i++)
	{
		if(cmt[i].lpn == INVALID)
			break;
		else if(cmt[i].lpn % 8 == bank)
		{
			offset = cmt[i].lpn / NUM_BANKS;
			write_dram_32(GC_BUF(offset / MAPPINGS_PER_PAGE) + sizeof(UINT32) * (offset % MAPPINGS_PER_PAGE), cmt[i].vpn);
		}
	}
}

static UINT32 gc_get_vpn(UINT32 const lpn)
{
	UINT32 offset = lpn / NUM_BANKS;
	UINT32 gtd_index = offset / MAPPINGS_PER_PAGE;

	return read_dram_32(GC_BUF(gtd_index) + sizeof(UINT32) * (offset % MAPPINGS_PER_PAGE));
}

static void gc_set_vpn(UINT32 const lpn, UINT32 const vpn)
{
	UINT32 offset = lpn / NUM_BANKS;
	UINT32 gtd_index = offset / MAPPINGS_PER_PAGE;

	write_dram_32(GC_BUF(gtd_index) + sizeof(UINT32) * (offset % MAPPINGS_PER_PAGE), vpn);
}

static void save_pmt(UINT32 bank)
{
	UINT32 i;
	UINT32 offset;
	UINT32 new_vpn;

	/*
	 * cache에 업데이트
	 */
	for(i=0; i<CMT_SIZE; i++)
	{
		if(cmt[i].lpn == INVALID)
			break;
		else if(cmt[i].lpn % 8 == bank)
		{
			offset = cmt[i].lpn / NUM_BANKS;
			cmt[i].vpn = read_dram_32(GC_BUF(offset / MAPPINGS_PER_PAGE) + sizeof(UINT32) * (offset % MAPPINGS_PER_PAGE));
		}
	}

	for(i=0; i<GTD_SIZE_PER_BANK; i++)
	{
		new_vpn = assign_new_map_write_vpn(bank);
		save_pm++;
		nand_page_program(bank,
				new_vpn / PAGES_PER_BLK,
				new_vpn % PAGES_PER_BLK,
				GC_BUF(i));
		gtd[bank][i] = new_vpn;
	}
}

static UINT32 assign_new_map_write_vpn(UINT32 const bank)
{
    ASSERT(bank < NUM_BANKS);

    UINT32 write_vpn;
    UINT32 vblock;
    UINT32 new_vblock;


    write_vpn = get_cur_map_write_vpn(bank);
    vblock    = write_vpn / PAGES_PER_BLK;

   // uart_printf("assign new map vpn at bank %d vblock 0x%x",bank, vblock);

    // NOTE: if next new write page's offset is
    // the last page offset of vblock (i.e. PAGES_PER_BLK - 1),
    if ((write_vpn % PAGES_PER_BLK) == (PAGES_PER_BLK - 1))
    {
        if(vblock == map_blk[bank][0])
        {
        	new_vblock = map_blk[bank][1];
        }
        else if(vblock == map_blk[bank][1])
        {
        	new_vblock = map_blk[bank][0];
        }
        else
        {
        	//uart_printf("assign new map write vpn some wrong");
        	while(1);
        }
        /*
         * valid한 gtd page들을 새로운 블락에 복사함
         */
    	UINT32 free_offset = 0;
    	UINT32 index;
    	for(index = 0; index<GTD_SIZE_PER_BANK; index++)
    	{
    		if(gtd[bank][index] != INVALID)
    		{
    			nand_page_copyback(bank,
    					vblock,
    					gtd[bank][index] % PAGES_PER_BLK,
    					new_vblock,
    					free_offset);
    			gtd[bank][index] = new_vblock * PAGES_PER_BLK + free_offset;
    			free_offset++;
    		}
    	}
    	/*
    	 * erase
    	 */
    	erase++;
    	nand_block_erase(bank, vblock);
    	write_vpn = new_vblock*PAGES_PER_BLK + free_offset;
    }
    else
    {
    	write_vpn++;
    }
	set_new_map_write_vpn(bank, write_vpn);
	return write_vpn;
}
