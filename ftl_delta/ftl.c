//사용법
//함수명 리스트를 적고, 그 다음 똑같은 순서로 함수명을 다시 적은뒤 그 함수들을 코딩하자
//
//함수 작성 완료되면 함수 정의부분의 주석에 "complte" 추가하자


#include "jasmine.h"
#include "lzf.h"


#define MAX_COMPRESS_SIZE	(BYTES_PER_PAGE / 2)//2048				//MAX COMPRESS SIZE : 2K(50%)
#define META_COUNT			10
#define MIN_RSRV_BLK		5

#define VAL	0x7fff
#define INVAL	0x8000

// SATA read/write buffer pointer id
UINT32 				  g_ftl_read_buf_id;
UINT32 				  g_ftl_write_buf_id;

UINT32 next_delta_offset[NUM_BANKS];						//next delta offset
UINT32 next_delta_meta[NUM_BANKS];						//next delta metadata

UINT16 lfsr = 0xACE1u;
UINT16 bit;

#define get_pbn(ppn)		((ppn) / PAGES_PER_BLK)
#define get_offset(ppn)		((ppn) % PAGES_PER_BLK)

//function list
static void format(void);

//open stream function
static void sanity_check(void);
static void build_bad_blk_list(void);
static void format(void);
static BOOL32 check_format_mark(void);				//in greedy
static void write_format_mark(void);				//in greedy
static void load_metadata(void);
static void init_metadata_sram(void);

//read stream function
UINT32 is_in_write_buffer(void);		//is in write buffer?
UINT32 is_in_cache(void);			//is in cache?
static void load_original_data(UINT32 const bank, UINT32 const ori_ppn, UINT32 const sect_offset, UINT32 const num_sectors_to_read);		//load original data

void read_from_delta(UINT32 const bank, UINT32 delta_ppn);		//read delta
UINT32 in_protected_region();		//was ppn in slru protected region (before pop)
UINT32 find_delta_data(UINT32 buf_ptr, UINT32 delta_ppn);		//find delta data in temp_buffer;
void _lzf_decompress (const void *const in_data, void *out_data);		//decompress data

//write stream function (not in read stream function)
static void evict(UINT32 const lpn, UINT32 const sect_offset, UINT32 const num_sectors);				//write(not in write buffer)
void load_original_data_write(UINT32 bank, UINT32 old_ppa, UINT32 page_offset, UINT32 num_sectors);		//load original data for write
UINT32 write_to_delta(UINT32 bank, UINT32 delta_ppn);		//write to delta write buffer
UINT32 get_free_page(UINT32 const bank);		//get free page
void save_original_data(UINT32 bank, UINT32 new_ppa, UINT32 page_offset, UINT32 column_cnt);		//write as original data
UINT32 _lzf_compress (const void *const in_data, void *out_data);		//compress by lzf
UINT32 is_remain_delta_buffer(UINT32 bank, UINT32 cs);	//is remain in delta_write_buffer?
void save_delta_page(UINT32 bank, UINT32 delta_ppn);		//save delta page in flash
void put_delta(UINT32 bank, UINT32 cs);			//put delta data in delta_write_buffer

//address related function
UINT32 set_valid_PPA(UINT32 PPA);		//set valid PPA
UINT32 set_invalid_PPA(UINT32 PPA);		//set invalid PPA
BOOL32 is_valid_PPA(UINT32 PPA);		//is valid PPA?

static UINT32 get_rsrv_pbn(UINT32 const bank, BOOL32 const gc);						//reserved block -> using block
static void ret_rsrv_pbn(UINT32 const bank, UINT32 const vblock);	//gc block -> reserved block

static UINT32 get_data_ppa(UINT32 const bank, UINT32 const lpa);	// get data ppa from data page mapping table
static void set_data_ppa(UINT32 const bank, UINT32 const lpa, UINT32 const ppa);	// set data ppa to data page mapping table

UINT32 rand();				//random number generation
static void garbage_collection(UINT32 const bank);		//garbage collection
void delta_copy(UINT32 bank, UINT32 lpa, UINT32 offset);	//delta copy
static BOOL32 is_in_delta_map(UINT32 const lpa, UINT32 const ppa);	//is in delta mapping?

/////////////////////////////////////////////

static UINT32		  g_bad_blk_count[NUM_BANKS];
static BOOL32 is_bad_block(UINT32 const bank, UINT32 const vblk_offset);
static void   set_bad_block(UINT32 const bank, UINT32 const vblk_offset);
#define set_mapblk_vpn(bank, mapblk_lbn, vpn) (g_misc_meta[bank].cur_mapblk_vpn[mapblk_lbn] = vpn)
static BOOL32         g_bsp_isr_flag[NUM_BANKS];
static misc_metadata  g_misc_meta[NUM_BANKS];
#define MISCBLK_VBN         0x1 // vblock #1 <- misc metadatavpn++)
#define set_miscblk_vpn(bank, vpn)           (g_misc_meta[bank].cur_miscblk_vpn = vpn)
#define CHECK_VPAGE(vpn)                     ASSERT((vpn) < (NUM_BANKS * VBLKS_PER_BANK * PAGES_PER_BLK))
#define get_delta_ppa(OFFSET)	read_dram_32(DELTA_PMT_ADDR + sizeof(UINT32) * (OFFSET * 2 + 1))
#define get_delta_lpa(OFFSET)	read_dram_32(DELTA_PMT_ADDR + sizeof(UINT32) * OFFSET * 2)
static void xor_buffer(const UINT32 src0, const UINT32 src1, const UINT32 dst);
static void merge(const UINT32 bank, const UINT32 lpa, const UINT32 ppa_delta);
UINT32 g_next_free_page[NUM_BANKS];

static void sanity_check(void)
{
    UINT32 dram_requirement = RD_BUF_BYTES + WR_BUF_BYTES + DRAM_BYTES_OTHER;

    if ((dram_requirement > DRAM_SIZE) ||
        (sizeof(misc_metadata) > BYTES_PER_PAGE)) {
        led_blink();
        while (1);
    }
}
static BOOL32 is_bad_block(UINT32 const bank, UINT32 const vblk_offset)
{
    if (tst_bit_dram(BAD_BLK_BMP_ADDR + bank*(VBLKS_PER_BANK/8 + 1), vblk_offset) == FALSE) {
        return FALSE;
    }
    return TRUE;
}
static void set_bad_block(UINT32 const bank, UINT32 const vblk_offset)
{
    set_bit_dram(BAD_BLK_BMP_ADDR + bank*(VBLKS_PER_BANK/8 + 1), vblk_offset);
    g_bad_blk_count[bank]++;
    uart_printf("found additional bad block: bank %d vblock %d", bank, vblk_offset);
}

//////////////////////////

//functions
//open stream function
void ftl_open(void)
{
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
 	if (check_format_mark() == FALSE)
//	if (TRUE)
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

static void format(void)
{
  //  uart_printf("Total FTL DRAM metadata size: %d KB", DRAM_BYTES_OTHER / 1024);
	/*
    uart_print("do format");
    uart_print("NUM_PSECTORS");
    uart_print_32(NUM_PSECTORS);
    uart_print("NUM_LSECTORS");
    uart_print_32(NUM_LSECTORS);
    uart_print("VBLKS_PER_BANK");
    uart_print_32(VBLKS_PER_BANK);
    uart_print("DATA_BLK_PER_BANK");
    uart_print_32(DATA_BLK_PER_BANK);
    uart_print("LOG_BLK_PER_BANK");
    uart_print_32(LOG_BLK_PER_BANK);
    uart_print("ISOL_BLK_PER_BANK");
    uart_print_32(ISOL_BLK_PER_BANK);
    uart_print("FREE_BLK_PER_BANK");
    uart_print_32(FREE_BLK_PER_BANK);
	*/

    //----------------------------------------
    // initialize DRAM metadata
    //----------------------------------------
    // data/log/isolation/free block mapping table

    mem_set_dram(DATA_PMT_ADDR, 0xFF, DATA_PMT_BYTES);
	mem_set_dram(DELTA_PMT_ADDR, 0xFF, DELTA_PMT_BYTES);
    mem_set_dram(RSRV_BMT_ADDR, NULL, RSRV_BMT_BYTES);

    // setting map/data/log/isolation/free block mapping table
    // NOTE: exclude bad blocks
    UINT32 lbn, vblock;
    for (UINT32 bank = 0; bank < NUM_BANKS; bank++) {
        vblock = MISCBLK_VBN;

        // misc. block (fixed location)
        nand_block_erase(bank, vblock);

        g_bsp_isr_flag[bank] = INVALID;
        // map block
        for (lbn = 0; lbn < MAP_BLK_PER_BANK;) {
            vblock++;
            if (is_bad_block(bank, vblock) == TRUE) {
                continue;
            }
            nand_block_erase_sync(bank, vblock);
            if (g_bsp_isr_flag[bank] != INVAL) {
                set_bad_block(bank, g_bsp_isr_flag[bank]);
                g_bsp_isr_flag[bank] = INVAL;
                continue;
            }
            set_mapblk_vpn(bank, lbn, vblock * PAGES_PER_BLK - 1);
            lbn++;
        }
        // rsrv block mapping table
        for (lbn = 0; lbn < RSRV_BLK_PER_BANK;) {
            vblock++;
            if (vblock >= VBLKS_PER_BANK) {
                break;
            }
            if (is_bad_block(bank, vblock) == TRUE) {
                continue;
            }
            nand_block_erase_sync(bank, vblock);
            if (g_bsp_isr_flag[bank] != INVAL) {
                set_bad_block(bank, g_bsp_isr_flag[bank]);
                g_bsp_isr_flag[bank] = INVAL;
                continue;
            }
            ret_rsrv_pbn(bank, vblock);
            lbn++;
        }
        uart_printf("above log blocks are invalid..bank %d lbn %d", bank, lbn);
        // set remained rsrv blocks as `invalid'
        /*
        while (lbn < LOG_BLK_PER_BANK) {
            write_dram_16(LOG_BMT_ADDR + ((bank * LOG_BLK_PER_BANK + lbn) * sizeof(UINT16)),
                          (UINT16)-1);
            lbn++;
        }
        */
    }
    //----------------------------------------
    // initialize SRAM metadata
    //----------------------------------------
    init_metadata_sram();

    // flush FTL metadata into NAND flash
    ftl_flush();

    write_format_mark();
	led(1);
    uart_print("format complete");
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


static void init_metadata_sram(void)
{
    for (UINT32 bank = 0; bank < NUM_BANKS; bank++) {
        set_miscblk_vpn(bank, (MISCBLK_VBN * PAGES_PER_BLK) - 1);

		UINT32 pbn = get_rsrv_pbn(bank, FALSE);
		g_next_free_page[bank] = pbn * PAGES_PER_BANK;
    }
}

static void build_bad_blk_list(void)
{
}

static void load_metadata(void)
{
}

//read stream function
void ftl_read(UINT32 const lba, UINT32 const num_sectors)
{
	//ref : greedy
	UINT32 remain_sects, num_sectors_to_read;
    UINT32 lpn, sect_offset;
    UINT32 bank, ppa;

    lpn          = lba / SECTORS_PER_PAGE;
    sect_offset  = lba % SECTORS_PER_PAGE;
    remain_sects = num_sectors;

	while(remain_sects != 0 )
	{

        if ((sect_offset + remain_sects) < SECTORS_PER_PAGE)
        {
            num_sectors_to_read = remain_sects;
        }
        else
        {
            num_sectors_to_read = SECTORS_PER_PAGE - sect_offset;
        }
        bank = lpn % NUM_BANKS;				//get_num_bank
        ppa  =  get_data_ppa(bank, lpn);	//ppa구함
        CHECK_VPAGE(ppn);

////////////////////////////////////////////////////////////////
		if (ppa != NULL)
		{
			if(is_in_write_buffer())	//is in write buffer?
			{
				//버퍼 내용 리턴
				//slru cache?
				return;
			}
			if(is_in_cache())			//is in cache?
			{
				//find ppn in cache
		
				load_original_data(bank, ppa, sect_offset, num_sectors_to_read);		//load original data
		
				if(is_valid_PPA(ppa))		//is the ppa has delta?
				{
					read_from_delta();	//read delta to temp2 buffer(use temp, temp2 buffer)
					//XOR operation
					if(in_protected_region())	//was ppa in slru protected region (before pop)
					{
						merge();		//write merge data
					}
					else
					{
						;
					}
				}
				else
				{
					;
				}
				//pop and push in first slru(protected) slot
			}
			else						//not in cache
			{
				//find ppn in page and make cache node -> 위에서 구해놓음(캐시 구현하면 밀어넣어야됨)
				//pop and push in first slru(probational) slot -> 일단없음
				load_original_data(bank, ppa, sect_offset, num_sectors_to_read);	//load original data -> 델타없는거니 nand read만하면될듯
			}
////////////////////////////////////////////////////////////////
		}
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

		sect_offset = 0;
		remain_sects -= num_sectors_to_read;
		lpn++;
	}

	return;
}

UINT32 is_in_write_buffer(void)		//is in write buffer?
{
	return 0;
}
UINT32 is_in_cache(void)			//is in cache?
{
	return 0;
}
static void load_original_data(UINT32 const bank, UINT32 const ori_ppn, UINT32 const sect_offset, UINT32 const num_sectors_to_read)		//load original data
{
	nand_page_ptread_to_host(bank, ori_ppn / PAGES_PER_BLK, ori_ppn % PAGES_PER_BLK, sect_offset, num_sectors_to_read);
}

void read_from_delta(UINT32 const bank, UINT32 delta_ppn)		//read delta to temp1 buffer
{
	UINT32 delta_read_start;			//pointer(start of delta)
	UINT32 pbn, offset;					//vbn of delta_ppn, offset of delta_ppn in vbn
	UINT32 buf_ptr;

	pbn = get_pbn(delta_ppn);
	offset = get_offset(delta_ppn);

	if(delta_ppn == NULL)
	{
		buf_ptr = DELTA_BUF(bank);
	}
	else
	{
		nand_page_read(bank, pbn, offset, TEMP_BUF_PTR(0));	//load from nand to temp_buffer
		buf_ptr = TEMP_BUF_PTR(0);
	}
	
	delta_read_start = find_delta_data(TEMP_BUF_PTR(2), delta_ppn);		//find delta data in temp_buffer;
	_lzf_decompress(delta_read_start, TEMP_BUF_PTR(1));

	return;
}

UINT32 in_protected_region()		//was ppn in slru protected region (before pop)
{
	return 0;
}

UINT32 find_delta_data(UINT32 buf_ptr, UINT32 delta_ppn)		//find delta data in temp_buffer;
{
	UINT32 lpn, offset;
	UINT32 i;

	buf_ptr = buf_ptr + 32;			//lpn starts at buf_ptr + 32

	for(i = 0; i < META_COUNT; i++)
	{
		lpn = read_dram_32(buf_ptr + i * 2 * sizeof(UINT32));
		if(find_ppn(lpn) == delta_ppn)
			break;
	}

	if(i == META_COUNT)
	{
		return -1;
	}
	else
	{
		return (buf_ptr + i * 2 * sizeof(UINT32));
	}
}

void _lzf_decompress (const void *const in_data, void *out_data)		//decompress data
{

	u8 *hreader;
	u8 *p;
	ssize_t rc3, bytes;
	UINT32 cs, us;

	hreader = in_data;
	p = hreader;

	cs = (hreader[0] << 8) | hreader[1];
	us = (hreader[2] << 8) | hreader[3];

	p = &hreader[TYPE1_HDR_SIZE];

	nr_read = cs + 4;

	if (lzf_decompress (p, cs, out_data, us) != us)
	{
		exit(1);
	}
}





//write stream function (not in read stream function)
void ftl_write(UINT32 const lba, UINT32 const num_sectors)
{
    UINT32 remain_sects, num_sectors_to_write;
    UINT32 lpn, sect_offset;

    lpn          = lba / SECTORS_PER_PAGE;
    sect_offset  = lba % SECTORS_PER_PAGE;
    remain_sects = num_sectors;
	while(remain_sects != 0)
	{
		if ((sect_offset + remain_sects) < SECTORS_PER_PAGE)
        {
            num_sectors_to_write = remain_sects;
        }
        else
        {
            num_sectors_to_write = SECTORS_PER_PAGE - sect_offset;
        }
////////////////////////////////////////////////////////////////
		if(is_in_write_buffer())	//is in write buffer?
		{
			//버퍼 내용 수정 후 리턴
			//slru cache?
			return;
		}
		else
		{
			evict(lpn, sect_offset, num_sectors_to_write);		//write(not in write buffer)
			//write input data in write buffer
		}
////////////////////////////////////////////////////////////////

		sect_offset = 0;
		remain_sects -= num_sectors_to_write;
        lpn++;
	}
}

static void evict(UINT32 const lpn, UINT32 const sect_offset, UINT32 const num_sectors)				//write(not in write buffer)
{	
	UINT32 bank, old_ppa, new_ppa;
    UINT32 vblock, page_num, page_offset, column_cnt;

    bank        = lpn % NUM_BANKS; // page striping
    page_offset = sect_offset;
    column_cnt  = num_sectors;

    old_ppa  = get_data_ppa(bank, lpn);

	if(is_in_cache())		//is in cache?
	{
		//find ppn in cache
		load_original_data_write(bank, old_ppa, sect_offset, num_sectors);		//load original data
		if(in_protected_region())	//was ppn in slru protected region (before pop)
		{
			//XOR
			if(write_to_delta() != NULL)	//write to delta write buffer
			{
				set_valid_PPA(old_ppa);		//이게 델타 썼을때 old_ppa맞나?
			}
			else				//write to delta fail
			{
				new_ppa = get_free_page(bank);	//get free page
				save_original_data(bank, new_ppa, page_offset, column_cnt);	//save original data
				set_data_ppa(bank, lpa, new_ppa);
			}
		}
		else
		{
			new_ppa = get_free_page(bank);		//get free page
			save_original_data();		//save original data
			set_data_ppa(bank, lpa, new_ppa);
		}
		
		//pop and push in first slru(protected) slot
		
	}
	else				//not in cache
	{
		//find ppn in page and make cache node ->이거도 위에서 구해놓음(캐시 구현하면 밀어넣어야됨)
		
		load_original_data_write(bank, old_ppa, sect_offset, num_sectors);			//load original data
		
		new_ppa = get_free_page(bank);				//get free page
		save_original_data(bank, new_ppa, page_offset, column_cnt);			//save original data
		set_data_ppa(bank, lpa, new_ppa);

		//pop and push in first slru(probational) slot
	}
	
}

/*
static void set_data_ppa(UINT32 const bank, UINT32 const lpa, UINT32 const ppa)
{
    write_dram_32(DATA_PMT_ADDR + ((bank * DATA_PAGES_PER_BANK + lpa) * sizeof(UINT32)), ppa);
}
*/

void load_original_data_write(UINT32 bank, UINT32 old_ppa, UINT32 page_offset, UINT32 num_sectors)		//load original data
{
	UINT32 vblock;
	UINT32 page_num;
    if (old_ppa != NULL)
    {
        vblock   = get_pbn(old_ppa);
        page_num = get_offset(old_ppa);

        if (num_sectors != SECTORS_PER_PAGE)
        {
            if ((num_sectors <= 8) && (page_offset != 0))
            {
                // one page async read
                nand_page_read(bank, vblock, page_num, FTL_BUF(bank));
                // copy `left hole sectors' into SATA write buffer
                if (page_offset != 0)
                {
                    mem_copy(WR_BUF_PTR(g_ftl_write_buf_id),
                             FTL_BUF(bank),
                             page_offset * BYTES_PER_SECTOR);
                }
                // copy `right hole sectors' into SATA write buffer
                if ((page_offset + num_sectors) < SECTORS_PER_PAGE)
                {
                    UINT32 const rhole_base = (page_offset + num_sectors) * BYTES_PER_SECTOR;

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
                    nand_page_ptread(bank,
                                     vblock,
                                     page_num,
                                     0,
                                     page_offset,
                                     WR_BUF_PTR(g_ftl_write_buf_id),
                                     RETURN_ON_ISSUE);
                }
                // read `right hole sectors'
                if ((page_offset + num_sectors) < SECTORS_PER_PAGE)
                {
                    nand_page_ptread(bank,
                                     vblock,
                                     page_num,
                                     page_offset + num_sectors,
                                     SECTORS_PER_PAGE - (page_offset + num_sectors),
                                     WR_BUF_PTR(g_ftl_write_buf_id),
                                     RETURN_ON_ISSUE);
                }
            }
        }
        // full page write
        page_offset = 0;
        num_sectors  = SECTORS_PER_PAGE;
        // invalid old page (decrease vcount)
        set_vcount(bank, vblock, get_vcount(bank, vblock) - 1);
    }else
	{
		if (page_offset != 0)
		{
            mem_set_dram(WR_BUF_PTR(g_ftl_write_buf_id),
                        0,
                        page_offset * BYTES_PER_SECTOR);
        }
        // copy `right hole sectors' into SATA write buffer
        if ((page_offset + num_sectors) < SECTORS_PER_PAGE)
        {
            UINT32 const rhole_base = (page_offset + num_sectors) * BYTES_PER_SECTOR;

            mem_copy(WR_BUF_PTR(g_ftl_write_buf_id) + rhole_base,
                        0,
                        BYTES_PER_PAGE - rhole_base);
        }
	}
}


UINT32 write_to_delta(UINT32 bank, UINT32 delta_ppn)	//write to delta write buffer
{
	UINT32 inv_delta = -1;
	UINT32 cs;
	UINT32 delta_page;


	if((cs = _lzf_compress(TEMP_BUF(2), TEMP_BUF(1))) <= MAX_COMPRESS_SIZE)			//try lzf compress and compressed
	{
		if(is_remain_delta_buffer(bank, cs))	//is remain delta_write_buffer?
		{
			inv_delta = find_delta_data(TEMP_BUF(1), delta_ppn);
			if(inv_delta != -1)
			{
				write_dram_32(inv_delta, -1);	//invalid prev delta
			}
			put_delta(bank, cs);					//put compressed delta in delta write buffer
			return 0;
		}
		else								//not remain delta_write_buffer
		{
			delta_page = get_free_page(bank);				//get free page
			save_delta_page(bank, delta_page);				//save delta page
			next_delta_meta[bank] = DELTA_BUF + sizeof(UINT32);		//initialize delta and meta pointer
			next_delta_data[bank] = DELTA_BUF + (2 * META_COUNT + 1) * sizeof(UINT32)
			put_delta(bank, cs);					//put compressed delta in delta write buffer
			return 0;
		}
	}
	else								//not compressed
	{
		return -1;
	}
}

UINT32 get_free_page(UINT32 const bank)				//get free page
{
	//ftl_open 할 때
	//free block 하나 잡아서 g_next_free_page 세팅 해줘야 함

	g_next_free_page[bank]++;

	//if(g_next_free_page[bank] == 블록의 마지막 페이지)
	if((g_next_free_page[bank]+1) % PAGES_PER_BLK == 0)
	{
		nand_page_program(bank, g_next_free_page[bank] / PAGES_PER_BLK, PAGES_PER_BLK - 1, LPA_BUF(bank));

		UINT32 pbn = get_rsrv_pbn(bank);
		g_next_free_page[bank] = pbn * PAGES_PER_BLK;
		/*
		버퍼 하나 잡아서
		g_lpns_current_blk[bank] 카피
		write to nand(버퍼, g_next_free_page[bank])

		get_rsrv_vbn[bank];

		g_next_free_page[bank] = 새로 가져온 블락의 첫 페이지
		*/
	}

	return g_next_free_page[bank];
}

void save_original_data(UINT32 bank, UINT32 new_ppa, UINT32 page_offset, UINT32 column_cnt)			//write as original data
{
	UINT32 vblock = new_vpn / PAGES_PER_BLK;
	UINT32 page_num = new_vpn % PAGES_PER_BLK;
	nand_page_ptprogram_from_host(bank, vblock, page_numl, page_offset, column_cnt);
}

UINT32 _lzf_compress (const void *const in_data, void *out_data)
{
	UINT32 i;
	UINT32 cs;
	UINT32 len;
	u8* header;

	cs = lzf_compress (in_data + MAX_HDR_SIZE, PAGE_SIZE, out_data + MAX_HDR_SIZE, PAGE_SIZE - 4);
	if (cs < CS_SIZE && cs > 0)
	{
		header = &out_data[MAX_HDR_SIZE - TYPE1_HDR_SIZE];
		header[0] = cs >> 8;
		header[1] = cs & 0xff;
		header[2] = in_len >> 8;
		header[3] = in_len & 0xff;
		len = cs + TYPE1_HDR_SIZE;
	}
	else
	{                       // write uncompressed
		return 0;
	}

	return cs;
}

UINT32 is_remain_delta_buffer(UINT32 bank, UINT32 cs)	//is remain in delta_buffer?
{
	if((next_delta_offset[bank] + cs) > PAGE_SIZE)
	{
		return 0;
	}
	if(next_delta_meta[bank] / (2 * sizeof(UINT32)) == META_COUNT)
	{
		return 0;
	}

	return 1;

}

void save_delta_page(UINT32 bank, UINT32 delta_ppn)		//save delta page in flash
{
	UINT32 i;
	UINT32 lpn, ppn;
	UINT32 pbn, offset;
	for(i = 0; i < META_SIZE; i++)
	{
		lpn = read_dram_32(DELTA_BUF(bank) + i * 2 * sizeof(UINT32));
		ppn = find_ppn(lpn);
		//assign delta ppn page to ppn->delta_ppn
	}
	pbn = get_pbn(delta_ppn);
	offset = get_offset(delta_ppn);
	nand_page_program(bank, pbn, offset, DELTA_BUF(bank));
}


void put_delta(UINT32 bank, UINT32 cs)			//put delta data in delta_buffer
{
	UINT16 header;
	write_dram_16(next_delta_meta[bank], cs);
	write_dram_16(next_delta_meta[bank] + 16, PAGE_SIZE);
	next_delta_meta[bank] = next_delta_meta[bank] + 16;
	mem_copy(next_delta_offset[bank], TEMP_BUF(1), cs);
	next_delta_offset[bank] = next_delta_offset[bank] + (cs + sizeof(UINT32)-1) / sizeof(UINT32) * sizeof(UINT32);
}




UINT32 set_valid_PPA(UINT32 const PPA)
{
	return VAL & PPA;
}

UINT32 set_invalid_PPA(UINT32 const PPA)
{
	return INVAL | PPA;
}

BOOL32 is_valid_PPA(UINT32 PPA)
{
	if(INVAL & PPA)
	{
		return FALSE;
	}
	else
		return TRUE;
}


static UINT32 get_rsrv_pbn(UINT32 const bank, BOOL32 const gc)
{
	if(gc == FALSE)	//if gc context, no checking rsrv cnt
		if(g_misc_meta[bank].rsrv_blk_cnt < MIN_RSRV_BLK)
		{
			//gc
			garbage_collection(bank);
		}
    //ASSERT(g_misc_meta[bank].rsrv_blk_cnt > 0);

    UINT32 rsrv_blk_offset = g_misc_meta[bank].rsrv_list_tail;
    g_misc_meta[bank].rsrv_list_tail = (rsrv_blk_offset + 1) % RSRV_BLK_PER_BANK;
    g_misc_meta[bank].rsrv_blk_cnt--;
    return read_dram_16(RSRV_BMT_ADDR + ((bank * RSRV_BLK_PER_BANK)+ rsrv_blk_offset) * sizeof(UINT16));
}
static void ret_rsrv_pbn(UINT32 const bank, UINT32 const vblock)
{
    ASSERT(g_misc_meta[bank].rsrv_blk_cnt <= RSRV_BLK_PER_BANK);
    ASSERT(vblock < VBLKS_PER_BANK);
    ASSERT(is_bad_block(bank, vblock) == FALSE);

    UINT32 rsrv_blk_offset = g_misc_meta[bank].rsrv_list_head;
    write_dram_16(RSRV_BMT_ADDR + ((bank * RSRV_BLK_PER_BANK)+ rsrv_blk_offset) * sizeof(UINT16), vblock);
    g_misc_meta[bank].rsrv_list_head = (rsrv_blk_offset + 1) % RSRV_BLK_PER_BANK;
    g_misc_meta[bank].rsrv_blk_cnt++;
}

// get data ppa from data page mapping table
static UINT32 get_data_ppa(UINT32 const bank, UINT32 const lpa)
{
    ASSERT(lpa < DATA_PAGES_PER_BANK);

    return read_dram_32(DATA_PMT_ADDR + ((bank * DATA_PAGES_PER_BANK + lpa) * sizeof(UINT32)));
}
// set data ppa to data page mapping table
static void set_data_ppa(UINT32 const bank, UINT32 const lpa, UINT32 const ppa)
{
    ASSERT(lpa < DATA_BLK_PER_BANK);
    ASSERT(ppa < VBLKS_PER_BANK * PAGES_PER_BLK);

    write_dram_32(DATA_PMT_ADDR + ((bank * DATA_PAGES_PER_BANK + lpa) * sizeof(UINT32)), ppa);
}

UINT32 rand()
{
	bit = ((lfsr >> 0) ^ (lfsr >> 2) ^ (lfsr >> 3) ^ (lfsr >> 5) ) & 1;
	return lfsr =  (lfsr >> 1) | (bit << 15);
}

#define is_data_page(lpa) is_valid_PPA(lpa)

static void garbage_collection(UINT32 const bank)
{
	UINT32 offset;				//offset : page offset of victim block
	UINT32 victim;

	UINT32 lpa, ppa;

	UINT32 delta_offset;			//delta_offset : offset of delta
	UINT32 delta_cnt;

	//빈블락 하나 뺐어오자
	UINT32 new_blk = get_rsrv_pbn(bank, TRUE);
	g_next_free_page[bank] = new_blk * PAGES_PER_BLK;

	do
	{
		//Data Page Mapping Table에서 하나 가져와서
		//그 블락을 싹 비울거야
		victim = (((UINT32)rand() * (UINT32)rand()) % DATA_PAGES_PER_BANK);
		victim = get_data_ppa(bank, victim);

		//lpa를 가져왔는데
		//아직 매핑이 안된 놈이 있을 수 있찌
	} while(victim == 0);

	victim = get_pbn(victim);

	//마지막페이지를 읽어오자
	nand_page_read(bank, victim, PAGES_PER_BLK - 1, GC_BUF_PTR(0));

	/*
	 * LPA들을 읽어오자
	 * LPA 종류
	 * 1. 오리지널 페이지 - LPA 저장
	 * 2. 델타 페이지 - 0x8000 저장
	 */
	for(offset = 0; offset < PAGES_PER_BLK - 1; offset++)
	{
		lpa = read_dram_32(GC_BUF_PTR(0) + sizeof(UINT32) * offset);

		if(is_data_page(lpa) == TRUE)
		{
			//data page라면

			//Page Mapping에서 LPA에 해당하는 PPA를 찾자
			ppa = get_data_ppa(bank, lpa);

			//요놈이 최신?? 매핑에 있는놈인가??
			if((set_valid_PPA(ppa) == victim * PAGES_PER_BLK + offset) || (set_invalid_PPA(ppa) == victim * PAGES_PER_BLK + offset))
			{
				//밸리드한놈은 복사
				//target_ppa = get_free_page(bank);
				nand_page_copyback(bank, victim, offset, get_pbn(g_next_free_page[bank]), get_offset(g_next_free_page[bank]));

				//매핑테이블 업데이트~
				//set_data_ppa(bank, lpa, g_next_free_page[bank]);
				if(is_valid_PPA(ppa))
				{
					set_data_ppa(bank, lpa, g_next_free_page[bank]);
				}
				else
				{
					set_data_ppa(bank, lpa, set_invalid_PPA(g_next_free_page[bank]));
				}

				//LPA page에 LPA 써줘야지~
				write_dram_32(LPA_BUF(0) + sizeof(UINT32) * get_offset(g_next_free_page[bank]), lpa);
				g_next_free_page[bank]++;
			}
			else
			{
				//mapping에 없으면 인밸리드... 그냥 지나감
				continue;
			}
		}
		else
		{
			//delta page 라면
			//일단 페이지를 읽어오고
			//하나하나 메타데이터로 요놈이 밸리드인지 알아내야행ㅠㅠ
			nand_page_read(bank, victim, offset, GC_BUF_PTR(1));	//GC버퍼로 옮김

			//델타가 몇개 들어있는지 알아냄
			delta_cnt = read_dram_32(GC_BUF_PTR(1));

			for(delta_offset = 0; delta_offset < delta_cnt; delta_offset++)
			{
				lpa = read_dram_32(GC_BUF_PTR(1) + sizeof(UINT32) * (delta_offset+1) * 2);
				/*
				 * //delta page meta에서 lpa를 하나 읽어왔어
				//요놈은
				//1. 밸리드한 델타
				 * 	밸리드한 델타와 메타를 델타버퍼에 복사
				//2. 인밸리드한 델타(버퍼에서 플래시 가기 전에 또 같은 lpa에 write 와서 이전놈이 인밸리드 된거
				 * 	요놈은 인밸리드(0x8000)을 저장해 놨어야해
				 *
				 * */

				if(lpa != INVAL)
				{
					//INVALID가 아니라도 쓰고 나서 또 바뀌었을 수 있으니까 첵첵!
					if(is_in_delta_map(lpa, victim * PAGES_PER_BLK + offset) == TRUE)
					{
						//요놈이 진짜 밸리드한 델타임
						/*
						 * 오프셋 가져가서 압축 풀고... WriteToDelta로 넘겨버리자
						 * -> 압축안풀고 바로 카피했다
						 * 요부분은 너가 좀 해줘!
						 * -> ㅇㅇ
						 */
						/*
						 * offset 정보는 얻는데, 압축을 풀필요는 없을거같아
						 * 압축안풀고 그 자료 그대로 옮기는게 delta_copy
						 * ;
						*/

						delta_data_offset = read_dram_32(GC_BUF_PTR(1) + sizeof(UINT32) * ((delta_offset + 1) * 2 + 1));
						delta_copy(bank, lpa, delta_data_offset);
					}
					else
					{
						//결국 인밸리드함 ㅠㅠ
						continue;
					}
				}
				else				//invalid delta
				{
					continue;
				}
			}

		}
	}
	//블락에 밸리드한거 새로운거에 다 복사함
	//이제 요놈 지우고 rsrv로 놔줘야해
	nand_block_erase(bank, victim);
	ret_rsrv_pbn(bank, victim);
}


void delta_copy(UINT32 bank, UINT32 lpa, UINT32 offset)
{
	UINT16 headergc;
	UINT16 cs;
	UINT32 data_cnt, data_offset;
	UINT32 i;
	data_cnt = read_dram_32(TEMP_BUF(1));
	for(i = 0; i < data_cnt; i++)
	{
		if(lpa == read_dram_32(GC_BUF_PTR(1) + sizeof(UINT32) * (i+1) * 2))
		{
			write_dram_32(GC_BUF_PTR(1) + sizeof(UINT32) * (i+1) * 2, INVAL);
		}
	}
	data_offset = read_dram_32(GC_BUF_PTR(1) + sizeof(UINT32) * (data_cnt * 2 + 1));
	cs = read_dram_16(GC_BUF_PTR(1) + data_offset);
	if(is_remain_delta_buffer(bank, cs))
	{
		mem_copy(next_delta_offset[bank], GC_BUF_PTR(1) + data_offset, cs);
		next_delta_offset[bank] = next_delta_offset[bank] + (cs + sizeof(UINT32)-1) / sizeof(UINT32) * sizeof(UINT32);
		write_dram_16(next_delta_meta[bank], lpa);
		write_dram_16(next_delta_meta[bank] + 16, offset);
		next_delta_meta[bank] = next_delta_meta[bank] + 32;
	}
	else
	{
		//get new page?
	}
}

static BOOL32 is_in_delta_map(UINT32 const lpa, UINT32 const ppa)
{
	UINT32 delta_page = 0;

	for(delta_page = 0; NUM_MAX_DELTA_PAGES_PER_BANK; delta_page++)
	{
		//delta page mapping table에서 요놈을 찾음
		if(lpa == get_delta_lpa(delta_page))
		{
			//lpa가 같은 놈인지를 찾았으면
			//ppa도 같은지를 확인해야지~
			if(ppa == get_delta_ppa(delta_page))
				return TRUE;
			else
				return FALSE;
		}
	}
	//못찾음
	return FALSE;
}

UINT32 g_delta_pmt_pointer;

static UINT32 get_ppa_delta(UINT32 const bank, UINT32 const lpa)
{
	INT32 offset = g_delta_pmt_pointer;
	for(; offset != g_delta_pmt_pointer + 1; offset = offset - 1)
	{
		if(offset == -1)
		{
			offset = NUM_MAX_DELTA_PAGES - 1;
		}
		
		if(lpa == read_dram_32(DELTA_PMT_ADDR + sizeof(UINT32) * 2 * offset))
		{
			return read_dram_32(DELTA_PMT_ADDR + sizeof(UINT32) * (2 * offset + 1));
		}
	}
	return INVAL;
}

//ㅋ캐시를 안쓰게 되면서
//merge하는 상황이 델타 매핑이 다 찼을때랑
//read 할 때 합친 값을 넘겨주는 상황 두가지임
//lpa랑 델타 ppa만 인자로 주면
//나머지는 여기서 알아서
//버퍼는 일단 템프버퍼로 할게
//나중에 코드 더 진행되야 템프버퍼 써도 되는지 못쓰는지 알 수 있을것 같아
static void merge(const UINT32 bank, const UINT32 lpa, const UINT32 ppa_delta)
{
	UINT32 ppa_ori = set_valid_PPA(get_data_ppa(bank, lpa));

	//델타랑 오리지널 읽어옴
	read_from_delta(bank, ppa_delta, lpa);
	nand_page_read(bank, get_pbn(ppa_ori), get_offset(ppa_ori), TEMP_BUF_PTR(0));

	//read_from_delta????

	//xor 해서 템프버프0에 저장
	xor_buffer(TEMP_BUF_PTR(0), TEMP_BUF_PTR(1), TEMP_BUF_PTR(0));
}

/*
 * 1. ftl_read에서 호출된 경우
 * 그냥 리턴하고 끝나면 됨
 * 2. 델타에다가 뭔가 쓰려고 할 때 쫓겨나는 경우
 * 리턴한 다음 다음을 수행
	UINT32 new_page = get_free_page(bank);
	nand_page_program(bank, get_pbn(ppa_ori), get_offset(ppa_ori), TEMP_BUF_PTR(0));
	set_data_ppa(bank, lpa, set_valid(ppa_ori));

	그러고 나서 델타 페이지 매핑에다가 새로 쓸 놈 쓰면 됨
*/

static void xor_buffer(const UINT32 src0, const UINT32 src1, const UINT32 dst)
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
