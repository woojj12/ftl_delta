//사용법
//함수명 리스트를 적고, 그 다음 똑같은 순서로 함수명을 다시 적은뒤 그 함수들을 코딩하자
//
//함수 작성 완료되면 함수 정의부분의 주석에 "complte" 추가하자


#include "jasmine.h"
#include "lzf.h"


#define MAX_COMPRESS_SIZE	(BYTES_PER_PAGE / 2)//2048				//MAX COMPRESS SIZE : 2K(50%)
#define MAX_DELTAS_PER_PAGE			10
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

#define get_pbn(ppa)		((ppa) / PAGES_PER_BLK)
#define get_offset(ppa)		((ppa) % PAGES_PER_BLK)

//function list
static void format(void);

//open stream function
static void sanity_check(void);
static void build_bad_blk_list(void);
static void format(void);
static void init_metadata_sram(void);

//read stream function
UINT32 is_in_write_buffer(void);		//is in write buffer?
UINT32 is_in_cache(void);			//is in cache?
static void load_original_data(UINT32 const bank, UINT32 const ori_ppa, UINT32 const buf_addr);

//void read_from_delta(UINT32 const bank, UINT32 delta_ppa);		//read delta
void read_from_delta(UINT32 const bank, UINT32 const lpa, UINT32 const delta_ppa, UINT32 const buf_addr);		//read delta to buf_addr
UINT32 in_protected_region();		//was ppa in slru protected region (before pop)
UINT32 find_delta_data(UINT32 buf_ptr, UINT32 delta_ppa);		//find delta data in temp_buffer;
void _lzf_decompress (const void *const in_data, void *out_data);		//decompress data

//write stream function (not in read stream function)
static void evict(UINT32 const lpa, UINT32 const sect_offset, UINT32 const num_sectors);				//write(not in write buffer)
void load_original_data_write(UINT32 bank, UINT32 old_ppa, UINT32 page_offset, UINT32 num_sectors);		//load original data for write
UINT32 write_to_delta(UINT32 bank, UINT32 delta_ppa);		//write to delta write buffer
UINT32 get_free_page(UINT32 const bank);		//get free page
void save_original_data(UINT32 bank, UINT32 new_ppa, UINT32 page_offset, UINT32 column_cnt);		//write as original data
UINT32 _lzf_compress (const void *const in_data, void *out_data);		//compress by lzf
UINT32 is_remain_delta_buffer(UINT32 bank, UINT32 cs);	//is remain in delta_write_buffer?
void save_delta_page(UINT32 bank, UINT32 delta_ppa);		//save delta page in flash
void put_delta(UINT32 bank, UINT32 cs);			//put delta data in delta_write_buffer

//address related function
UINT32 set_valid_ppa(UINT32 ppa);		//set valid ppa
UINT32 set_invalid_ppa(UINT32 ppa);		//set invalid ppa
BOOL32 is_valid_ppa(UINT32 ppa);		//is valid ppa?

static UINT32 get_rsrv_pbn(UINT32 const bank, BOOL32 const gc);						//reserved block -> using block
static void ret_rsrv_pbn(UINT32 const bank, UINT32 const vblock);	//gc block -> reserved block

static UINT32 get_data_ppa(UINT32 const bank, UINT32 const lpa);	// get data ppa from data page mapping table
static void set_data_ppa(UINT32 const bank, UINT32 const lpa, UINT32 const ppa);	// set data ppa to data page mapping table

UINT32 rand();				//random number generation
static void garbage_collection(UINT32 const bank);		//garbage collection
void delta_copy(UINT32 bank, UINT32 lpa, UINT32 offset);	//delta copy
//static BOOL32 is_in_delta_map(UINT32 const lpa, UINT32 const ppa);	//is in delta mapping?

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
#define set_delta_ppa(OFFSET, PPA)	write_dram_32(DELTA_PMT_ADDR + sizeof(UINT32) * (OFFSET * 2 + 1), PPA)
#define set_delta_lpa(OFFSET, LPA)	wirte_dram_32(DELTA_PMT_ADDR + sizeof(UINT32) * OFFSET * 2, LPA)
static void xor_buffer(UINT32 const src0, UINT32 const src1, UINT32 const dst);
static void merge(UINT32 const bank, UINT32 const lpa, UINT32 const ppa_delta, UINT32 const buf_ptr);
UINT32 g_next_free_page[NUM_BANKS];
UINT32 g_delta_pmt_pointer;
static UINT32 get_ppa_delta(UINT32 const lpa);

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
	//if (check_format_mark() == FALSE)
	if (TRUE)
	{
		uart_print("do format");
		format();
		uart_print("end format");
	}
	// load FTL metadata
	else
	{
		//load_metadata();
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
	//ftl_flush();

	//write_format_mark();
	led(1);
	uart_print("format complete");
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
	UINT32 bank, num_entries, result, vblk_offset;
	scan_list_t* scan_list = (scan_list_t*) TEMP_BUF_ADDR;

	mem_set_dram(BAD_BLK_BMP_ADDR, NULL, BAD_BLK_BMP_BYTES);

	disable_irq();

	flash_clear_irq();

	for (bank = 0; bank < NUM_BANKS; bank++) {
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

		if (BSP_INTR(bank) & FIRQ_DATA_CORRUPT) {
			result = FAIL;
		}
		else {
			UINT32 i;

			num_entries = read_dram_16(&(scan_list->num_entries));

			if (num_entries > SCAN_LIST_ITEMS) {
				result = FAIL;
			}
			else {
				for (i = 0; i < num_entries; i++) {
					UINT16 entry = read_dram_16(scan_list->list + i);
					UINT16 pblk_offset = entry & 0x7FFF;

					if (pblk_offset == 0 || pblk_offset >= PBLKS_PER_BANK) {
#if OPTION_REDUCED_CAPACITY == FALSE
						result = FAIL;
#endif
					}
					else {
						write_dram_16(scan_list->list + i, pblk_offset);
					}
				}
			}
		}
		if (result == FAIL) {
			num_entries = 0;
		}
		else {
			write_dram_16(&(scan_list->num_entries), 0);
		}
		g_bad_blk_count[bank] = 0;

		for (vblk_offset = 1; vblk_offset < VBLKS_PER_BANK; vblk_offset++) {
			BOOL32 bad = FALSE;

#if OPTION_2_PLANE
			{
				UINT32 pblk_offset;

				pblk_offset = vblk_offset * NUM_PLANES;

				if (mem_search_equ_dram(scan_list, sizeof(UINT16), num_entries + 1, pblk_offset) < num_entries + 1) {
					bad = TRUE;
				}
				pblk_offset = vblk_offset * NUM_PLANES + 1;

				if (mem_search_equ_dram(scan_list, sizeof(UINT16), num_entries + 1, pblk_offset) < num_entries + 1) {
					bad = TRUE;
				}
			}
#else
			{
				if (mem_search_equ_dram(scan_list, sizeof(UINT16), num_entries + 1, vblk_offset) < num_entries + 1)	{
					bad = TRUE;
				}
			}
#endif

			if (bad) {
				g_bad_blk_count[bank]++;
				set_bit_dram(BAD_BLK_BMP_ADDR + bank*(VBLKS_PER_BANK/8 + 1), vblk_offset);
			}
		}
	}
}

//read stream function
void ftl_read(UINT32 const lba, UINT32 const num_sectors)
{
	//ref : greedy
	UINT32 remain_sects, num_sectors_to_read;
	UINT32 lpa, sect_offset;
	UINT32 bank, ppa;

	lpa          = lba / SECTORS_PER_PAGE;
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
		bank = lpa % NUM_BANKS;				//get_num_bank
		ppa  =  get_data_ppa(bank, lpa);	//ppa구함
		CHECK_VPAGE(ppa);

		////////////////////////////////////////////////////////////////
		if (ppa != NULL)
		{
			if(is_valid_ppa(ppa) == TRUE)
			{
				//no delta
				load_original_data(bank, ppa, sect_offset, num_sectors_to_read, RD_BUF_PTR(g_ftl_read_buf_id));	//load original data -> 델타없는거니 nand read만하면될듯
			}
			else
			{
				//there is delta
				merge(bank, lpa, get_ppa_delta(lpa), RD_BUF_PTR(g_ftl_read_buf_id));
			}
			g_ftl_read_buf_id = (g_ftl_read_buf_id + 1) % NUM_RD_BUFFERS;
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
		lpa++;
	}

	return;
}

static void load_original_data(UINT32 const bank, UINT32 const ori_ppa, UINT32 const buf_addr)		//load original data
{
	//꼭 partial page로 읽어야할 이유가 있음???
	//nand_page_ptread_to_host(bank, ori_ppa / PAGES_PER_BLK, ori_ppa % PAGES_PER_BLK, sect_offset, num_sectors_to_read);
	nand_page_read(bank, get_pbn(ori_ppa), get_offset(ori_ppa), buf_addr);
}

void read_from_delta(UINT32 const bank, UINT32 const lpa, UINT32 const delta_ppa, UINT32 const dst_buf)		//read delta to buf_addr
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

UINT32 find_delta_data(UINT32 const buf_ptr, UINT32 const lpa)		//find delta data in temp_buffer;
{
	UINT32 offset;
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

void _lzf_decompress (const void *const in_data, void *out_data)		//decompress data
{

	//in_data -> out_data
	/*
	 * 압축을 푼다
	 *
	 */
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
	UINT32 lpa, sect_offset;

	lpa          = lba / SECTORS_PER_PAGE;
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
		{
			evict(lpa, sect_offset, num_sectors_to_write);		//write(not in write buffer)
			//write input data in write buffer
		}
		////////////////////////////////////////////////////////////////

		sect_offset = 0;
		remain_sects -= num_sectors_to_write;
		lpa++;
	}
}

static void evict(UINT32 const lpa, UINT32 const sect_offset, UINT32 const num_sectors)				//write(not in write buffer)
{
	UINT32 bank, old_ppa, new_ppa;
	UINT32 vblock, page_num, page_offset, column_cnt;

	bank        = lpa % NUM_BANKS; // page striping
	page_offset = sect_offset;
	column_cnt  = num_sectors;

	//이번 매핑 읽어옴
	old_ppa  = get_data_ppa(bank, lpa);

	if(old_ppa != NULL)
	{
		/*
		 * 썼던적이 있네~
		 * 압축해봐야지
		 */
		nand_page_read(bank, get_pbn(old_ppa), get_offset(old_ppa), TEMP_BUF_PTR(0));

		/*
		 * handling hole
		 */
		if(num_sectors != SECTORS_PER_PAGE)
		{
			if(page_offset != 0)
			{
				mem_copy(WR_BUF_PTR(g_ftl_write_buf_id), TEMP_BUF_PTR(0), page_offset * BYTES_PER_SECTOR);
			}
			if((page_offset + num_sectors) < SECTORS_PER_PAGE)
			{
				UINT32 const rhole_base = (page_offset + num_sectors) * BYTES_PER_SECTOR;
				mem_copy(WR_BUF_PTR(g_ftl_write_buf_id) + rhole_base, TEMP_BUF_PTR(0), BYTES_PER_PAGE - rhole_base);
			}
		}
		/*
		 * 압축하는 함수
		 * compress
		 * 인자 : 버퍼 두개
		 * 리턴 : TRUE/FALSE
		 * 성공하면 TEMP_BUF_PTR(2)에 압축한 것이 들어감
		 */
		BOOL32 comp_success = compress(TEMP_BUF_PTR(0), WD_BUF_PTR(g_ftl_write_buf_id));

		if(comp_success == TRUE)
		{
			//압축 성공
			//데이터매핑테이블 업데이트
			old_ppa = set_invalid(old_ppa);
			set_data_ppa(bank, lpa, old_ppa);
			//델타매핑 쓸 위치선정
			UINT32 delta_page_map_offset = get_next_delta_table_space(bank, lpa, 0);

			/*
			 * 델타 매핑 테이블에다가 엔트리 업데이트
			 * lpa는 그냥 써 주고
			 * ppa는 0을 씀
			 * 0은 델타 버퍼에 있다는 의미
			 * 그리고 압축한 것은 write to delta
			 */
			set_delta_lpa(delta_page_map_offset, lpa);
			set_delta_ppa(delta_page_map_offset, 0);
			/*
			 * 델타에 쓰는 함수
			 * write_to_delta
			 * 인자 : bank, lpa, buf_ptr
			 * buf_ptr은 압축한 놈이 저장된 버퍼
			 * 압축한 놈을 델타버퍼에 집어넣음
			 * 꽉차면 알아서 낸드에 씀
			 */
			write_to_delta(bank, lpa, TEMP_BUF_PTR(2));
		}
		else
		{
			//압축 실패
			goto WRITE_ORIGINAL;
		}
	}
	else
	{
		/*
		 * 한번도 안썼음
		 * 그냥 씀
		 *
		 * 쓸 때 해야할 일
		 * 새 페이지 받아옴
		 * 페이지 매핑 바꿔줌
		 * 낸드 프로그램
		 * 블록당 마지막 페이지에 쓸 LPA 페이지에 현재꺼 업데이트
		 */
		//handling hole
		if(page_offset != 0)
			mem_set_dram(WR_BUF_PTR(g_ftl_write_buf_id), 0, page_offset * BYTES_PER_SECTOR);
		if((page_offset + num_sectors) < SECTORS_PER_PAGE)
		{
			UINT32 const rhole_base = (page_offset + num_sectors) * BYTES_PER_SECTOR;
			mem_set_dram(WR_BUF_PTR(g_ftl_write_buf_id), 0, BYTES_PER_PAGE - rhole_base);
		}

		WRITE_ORIGINAL:
		new_ppa = get_free_page(bank);
		set_data_ppa(bank, lpa, new_ppa);
		nand_page_program(bank, get_pbn(new_ppa), get_offset(new_ppa), WD_BUF_PTR(g_ftl_write_buf_id));
		write_dram_32(LPA_BUF(bank) + sizeof(UINT32) * get_offset(g_next_free_page[bank]), lpa);
	}
}

static UINT32 get_next_delta_table_space(UINT32 const bank, UINT32 const lpa, UINT32 new_ppa)
{
	UINT32 offset = (g_delta_pmt_pointer + 1) % NUM_MAX_DELTA_PAGES;
	//g_delta_pmt_pointer = (g_delta_pmt_pointer + 1) % NUM_MAX_DELTA_PAGES;
	UINT32 old_delta_ppa = get_delta_ppa(g_delta_pmt_pointer);
	UINT32 old_delta_lpa = get_delta_lpa(g_delta_pmt_pointer);
	if((is_valid_ppa(old_delta_ppa) == FALSE)\
			|| (old_delta_lpa == lpa))
	{
		/*
		 * 지금 포인터가 가리키는 엔트리가 인밸리드한 놈이면
		 * 아니면 지금 포인터가 가리키는 엔트리가 지금 쓰려고 하는놈이랑 같은 lpa이면
		 * 그 위치에다 그냥 덮어 쓰면 됨
		 */
		g_delta_pmt_pointer = offset;
		return g_delta_pmt_pointer;
	}
	else// if(get_delta_lpa(offset) != lpa)
	{
		/*
		 * 지금 포인터가 가리키는 엔트리가 밸리드하면
		 * 그놈을 쫓아보내야함
		 * merge해서
		 * 다시 씀
		 */
		//쫓겨나는놈 써야함
		if(new_ppa == 0)
		{
			//정상적인 경로
			new_ppa = get_free_page(bank);
		}
		else
		{
			//재귀적으로 호출된 경우
			;
		}

		if(offset != (g_delta_pmt_pointer + 1) % NUM_MAX_DELTA_PAGES)
		{
			/*
			 * 뭔가 뭔가에 의해서 겟 프리피피에이를 갔다 왔더니
			 * delta 매핑의 포인터가 바뀌어 있음
			 * 아마도 gc하면서 델타가 쫓겨날 때
			 * 델타 매핑을 바꾼 경우임
			 * 지금껏 하던거 그냥 똥되고
			 * 그냥 이 함수 재귀적으로 콜!
			 */
			return get_next_delta_table_space(bank, lpa, new_ppa);
		}
		else
		{
			/*
			 * 기존에 있던놈 머지해서 쫓아내자~
			 */
			set_data_ppa(bank, old_delta_lpa, new_ppa);
			merge(bank, old_delta_lpa, old_delta_ppa, TEMP_BUF_PTR(0));
			nand_page_program(bank, get_pbn(new_ppa), get_offset(new_ppa), TEMP_BUF_PTR(0));
			write_dram_32(LPA_BUF(bank) + sizeof(UINT32) * get_offset(new_ppa), old_delta_lpa);

			g_delta_pmt_pointer = offset;
			return g_delta_pmt_pointer;
		}
	}
}

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
			// left/right hole async read operation (two partial page read)
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
	else
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

UINT32 write_to_delta(UINT32 bank, UINT32 delta_ppa)	//write to delta write buffer
{
	UINT32 inv_delta = -1;
	UINT32 cs;
	UINT32 delta_page;


	if((cs = _lzf_compress(TEMP_BUF_PTR(2), TEMP_BUF_PTR(1))) <= MAX_COMPRESS_SIZE)			//try lzf compress and compressed
	{
		if(is_remain_delta_buffer(bank, cs))	//is remain delta_write_buffer?
		{
			inv_delta = find_delta_data(TEMP_BUF_PTR(1), delta_ppa);
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
			next_delta_meta[bank] = DELTA_BUF(bank) + sizeof(UINT32);		//initialize delta and meta pointer
			next_delta_offset[bank] = DELTA_BUF(bank) + (2 * MAX_DELTAS_PER_PAGE + 1) * sizeof(UINT32);
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

		UINT32 pbn = get_rsrv_pbn(bank, FALSE);
		g_next_free_page[bank] = pbn * PAGES_PER_BLK;
		/*
		버퍼 하나 잡아서
		g_lpas_current_blk[bank] 카피
		write to nand(버퍼, g_next_free_page[bank])

		get_rsrv_vbn[bank];

		g_next_free_page[bank] = 새로 가져온 블락의 첫 페이지
		 */
	}

	return g_next_free_page[bank];
}

void save_original_data(UINT32 const bank, UINT32 const new_ppa)			//write as original data
{
	UINT32 vblock = new_ppa / PAGES_PER_BLK;
	UINT32 page_num = new_ppa % PAGES_PER_BLK;
	nand_page_program_from_host(bank, vblock, page_num);
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
	if(next_delta_meta[bank] / (2 * sizeof(UINT32)) == MAX_DELTAS_PER_PAGE)
	{
		return 0;
	}

	return 1;

}

void save_delta_page(UINT32 const bank, UINT32 const delta_ppa)		//save delta page in flash
{
	UINT32 i;
	UINT32 lpa, ppa;
	UINT32 pbn, offset;
	UINT32 cnt = read_dram_32(DELTA_BUF(bank));
	for(i = 0; i < cnt; i++)
	{
		lpa = read_dram_32(DELTA_BUF(bank) + (i * 2 + 1) * sizeof(UINT32));
		ppa = get_ppa_delta(lpa);
		//assign delta ppa page to ppa->delta_ppa
	}
	pbn = get_pbn(delta_ppa);
	offset = get_offset(delta_ppa);
	nand_page_program(bank, pbn, offset, DELTA_BUF(bank));
}

void put_delta(UINT32 bank, UINT32 const lpa, UINT32 const offset, UINT32 cs)			//put delta data in delta_buffer
{
	UINT16 header;
	write_dram_32(next_delta_meta[bank], offset);
	write_dram_32(next_delta_meta[bank] + sizeof(UINT32), lpa);
	next_delta_meta[bank] = next_delta_meta[bank] + sizeof(UINT32);


	mem_copy(next_delta_offset[bank], TEMP_BUF(1), cs);
	next_delta_offset[bank] = next_delta_offset[bank] + (cs + sizeof(UINT32)-1) / sizeof(UINT32) * sizeof(UINT32);
}

UINT32 set_valid_ppa(UINT32 const ppa)
{
	return VAL & ppa;
}

UINT32 set_invalid_ppa(UINT32 const ppa)
{
	return INVAL | ppa;
}

BOOL32 is_valid_ppa(UINT32 ppa)
{
	if(INVAL & ppa)
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

#define is_data_page(lpa) is_valid_ppa(lpa)

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

			//Page Mapping에서 LPA에 해당하는 ppa를 찾자
			ppa = get_data_ppa(bank, lpa);
			UINT32 ppa_current = victim * PAGES_PER_BLK + offset;

			//요놈이 최신?? 매핑에 있는놈인가??
			if((set_valid_ppa(ppa) == ppa_current) || (set_invalid_ppa(ppa) == ppa_current))
			{
				//밸리드한놈은 복사
				//target_ppa = get_free_page(bank);
				nand_page_copyback(bank, victim, offset, get_pbn(g_next_free_page[bank]), get_offset(g_next_free_page[bank]));

				//매핑테이블 업데이트~
				//set_data_ppa(bank, lpa, g_next_free_page[bank]);
				if(is_valid_ppa(ppa))
				{
					set_data_ppa(bank, lpa, g_next_free_page[bank]);
				}
				else
				{
					set_data_ppa(bank, lpa, set_invalid_ppa(g_next_free_page[bank]));
				}

				//LPA page에 LPA 써줘야지~
				write_dram_32(LPA_BUF(bank) + sizeof(UINT32) * get_offset(g_next_free_page[bank]), lpa);
				g_next_free_page[bank]++;
				ASSERT(g_next_free_page[bank] <= PAGES_PER_BLK);
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
					//if(is_in_delta_map(lpa, victim * PAGES_PER_BLK + offset) == TRUE)
					if(get_ppa_delta(lpa) == ppa)
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
						UINT32 delta_data_offset = read_dram_32(GC_BUF_PTR(1) + sizeof(UINT32) * ((delta_offset + 1) * 2 + 1));
						wirte_to_delta(bank, lpa, delta_data_offset);
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


void delta_copy(UINT32 bank, UINT32 lpa, UINT32 offset, UINT32 inbuf, UINT32 outbuf)
{
	UINT16 headergc;
	UINT16 cs;
	UINT32 data_cnt, data_offset;
	UINT32 i;
	UINT32 new_ppa;

	cs = read_dram_16(inbuf + offset);

	if(is_remain_delta_buffer(bank, cs))
	{
		write_dram_32(next_delta_meta[bank], lpa);
		write_dram_32(next_delta_meta[bank] + sizeof(UINT32), offset);
		mem_copy(next_delta_offset[bank], inbuf + offset, cs);
		//nand에 write시 data_cnt를 적어주는걸로 하자 -> 여긴안적음
	}
	else	//delta write buffer 공간부족
	{
		//nand에 write시 data_cnt를 적어주는걸로 하자 -> 여기 적어야됨
		data_cnt = ((next_delta_meta[bank] - DELTA_BUF) / sizeof(UINT32) - 1 ) / 2;
		write_dram_32(DELTA_BUF, data_cnt);

		UINT32 test_ppa = g_next_free_page[bank] + 1;
		new_ppa = get_free_page(bank);

		ASSERT(test_ppa == new_ppa);

		next_delta_meta[bank] = DELTA_BUF + sizeof(UINT32);
		next_delta_offset[bank] = DELTA_BUF +

	}
	/*
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
	 */
}

/*lpa를 받아서 델타 매핑을 뒤져서
 * 델타 ppa를 반환
 * 없으면 인밸
 */
static UINT32 get_ppa_delta(UINT32 const lpa)
{
	UINT32 offset = g_delta_pmt_pointer;
	for(; offset != g_delta_pmt_pointer + 1; offset = (offset - 1) % NUM_MAX_DELTA_PAGES)
	{
		if(lpa == get_delta_lpa(offset))//read_dram_32(DELTA_PMT_ADDR + sizeof(UINT32) * 2 * offset))
		{
			return get_delta_ppa(offset);//read_dram_32(DELTA_PMT_ADDR + sizeof(UINT32) * (2 * offset + 1));
		}
	}
	return INVAL;
}
//g_ftl_read_buf_id = (g_ftl_read_buf_id + 1) % NUM_RD_BUFFERS

//ㅋ캐시를 안쓰게 되면서
//merge하는 상황이 델타 매핑이 다 찼을때랑
//read 할 때 합친 값을 넘겨주는 상황 두가지임
//lpa랑 델타 ppa만 인자로 주면
//나머지는 여기서 알아서
//버퍼는 일단 템프버퍼로 할게
//나중에 코드 더 진행되야 템프버퍼 써도 되는지 못쓰는지 알 수 있을것 같아
static void merge(UINT32 const bank, UINT32 const lpa, UINT32 const ppa_delta, UINT32 const buf_ptr)
{
	ASSERT(ppa_delta != INVAL);
	UINT32 ppa_ori = set_valid_ppa(get_data_ppa(bank, lpa));

	//델타랑 오리지널 읽어옴
	read_from_delta(bank, lpa, ppa_delta, TEMP_BUF_PTR(1));
	nand_page_read(bank, get_pbn(ppa_ori), get_offset(ppa_ori), TEMP_BUF_PTR(0));

	//read_from_delta????

	//xor 해서 템프버프0에 저장
	xor_buffer(TEMP_BUF_PTR(0), TEMP_BUF_PTR(1), buf_ptr);
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
