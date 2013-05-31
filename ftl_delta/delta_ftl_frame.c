//사용법
//함수명 리스트를 적고, 그 다음 똑같은 순서로 함수명을 다시 적은뒤 그 함수들을 코딩하자
//
//함수 작성 완료되면 함수 정의부분의 주석에 "complte" 추가하자



//function list
//read stream function
ftl_read();
is_in_write_buffer();		//is in write buffer?
is_in_cache();			//is in cache?
load_original_data();		//load original data

read_from_delta();		//read delta
in_protected_region();		//was ppn in slru protected region (before pop)
copy_to_temp_buffer();		//copy from delta_write_buffer to temp_buffer (use temp1, temp2 buffer)
find_delta_data();		//find delta data in temp_buffer;
_lzf_decompress();		//decompress data

//write stream function (not in read stream function)
ftl_write();
evict();			//write(not in write buffer)
write_to_delta();		//write to delta write buffer
get_free_page();		//get free page
save_original_data();		//write as original data
_lzf_compress();		//compress by lzf
is_remain_delta_write_buffer();	//is remain in delta_write_buffer?
put_delta();			//put delta data in delta_write_buffer



//functions
//read stream function

UINT32 g_next_free_page[bank];

UINT32 get_free_page(bank)
{
	//ftl_open 할 때
	//free block 하나 잡아서 g_next_free_page 세팅 해줘야 함

	g_next_free_page[bank]++;

	//if(g_next_free_page[bank] == 블록의 마지막 페이지)
	if((g_next_free_page[bank]+1) % PAGES_PER_BLK == 0)
	{
		nand_page_program(bank, g_next_free_page / PAGES_PER_BLK, PAGES_PER_BLK - 1, LPN_BUF(bank));
		
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

UINT32 set_valid_PPA(UINT32 PPA)
{
	return 0x7fff & PPA_delta;
}

UINT32 set_invalid_PPA(UINT32 PPA)
{
	return 0x8000 & PPA_delta;
}

BOOL32 is_valid_PPA(UINT32 PPA)
{
	if(0x8000 & PPA)
	{
		return FALSE;
	}
	else
		return TRUE;
}

static void format(void)
{
    uart_printf("Total FTL DRAM metadata size: %d KB", DRAM_BYTES_OTHER / 1024);

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
    mem_set_dram(DATA_PMT_ADDR, NULL, DATA_PMT_BYTES);
	mem_set_dram(DELTA_PMT_ADDR, NULL, DELTA_PMT_BYTES);
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
            if (g_bsp_isr_flag[bank] != INVALID) {
                set_bad_block(bank, g_bsp_isr_flag[bank]);
                g_bsp_isr_flag[bank] = INVALID;
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
            if (g_bsp_isr_flag[bank] != INVALID) {
                set_bad_block(bank, g_bsp_isr_flag[bank]);
                g_bsp_isr_flag[bank] = INVALID;
                continue;
            }
            ret_free_vbn(bank, vblock);
            lbn++;
        }
        uart_printf("above log blocks are invalid..bank %d lbn %d", bank, lbn);
        // set remained rsrv blocks as `invalid'
        while (lbn < LOG_BLK_PER_BANK) {
            write_dram_16(LOG_BMT_ADDR + ((bank * LOG_BLK_PER_BANK + lbn) * sizeof(UINT16)),
                          (UINT16)-1);
            lbn++;
        }
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

static void init_metadata_sram(void)
{
    for (UINT32 bank = 0; bank < NUM_BANKS; bank++) {
        set_miscblk_vpn(bank, (MISCBLK_VBN * PAGES_PER_BLK) - 1);

		UINT32 pbn = get_rsrv_pbn(bank);
		g_next_free_page[bank] = pbn * PAGES_PER_BANK;
    }
}
static UINT32 get_rsrv_pbn(UINT32 const bank)
{
    ASSERT(g_misc_meta[bank].rsrv_blk_cnt > 0);

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

ftl_read()
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
		
		load_original_data();		//load original data
		
		if(is_valid_PPA())		//is the ppn has delta?
		{
			read_from_delta();	//read delta to temp2 buffer(use temp, temp2 buffer)
			//XOR operation
			if(in_protected_region())	//was ppn in slru protected region (before pop)
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
		//find ppn in page and make cache node
		//pop and push in first slru(probational) slot
		load_original_data();	//load original data
	}
	
	return;
}

UINT32 is_in_write_buffer()		//is in write buffer?
{
	return 0;
}
UINT32 is_in_cache()			//is in cache?
{
	return 0;
}
UINT32 load_original_data(UINT32 ori_ppn)		//load original data
{
	
}

void read_from_delta(UINT32 const bank, UINT32 delta_ppn)		//read delta to temp1 buffer
{
	UINT32 delta_read_start;			//pointer(start of delta)
	UINT32 pbn, offset;					//vbn of delta_ppn, offset of delta_ppn in vbn

	pbn = get_pbn(delta_ppn);
	offset = get_offset(delta_ppn);

	if(delta_ppn == NULL)
	{
		copy_to_temp_buffer(bank);	//copy from delta_buffer to temp_buffer2 (use temp1, temp2 buffer)
	}
	else
	{
		nand_page_read(bank, pbn, offset, TEMP_BUF_PTR(0));	//load from nand to temp_buffer
	}
	
	delta_read_start = find_delta_data(2, delta_ppn);		//find delta data in temp_buffer;
	_lzf_decompress(delta_read_start, TEMP_BUF_PTR(1));
/*
//	UINT32 decomp;
	decomp = _lzf_decompress(delta_read_start, TEMP_BUF_PTR(1));		//decompress data
	if(decomp != PAGE_SIZE)
	{
		uart_printf("not decompressed!");
	}
*/
	return;
}


in_protected_region()		//was ppn in slru protected region (before pop)
{
	return 0;
}
static inline void copy_to_temp_buffer(UINT32 bank);		//copy from delta_buffer to temp_buffer2
{
	mem_copy(TEMP_BUF_PTR(2), DELTA_BUF(bank), PAGE_SIZE);
}

UINT32 find_delta_data(UINT32 buf_num, UINT32 delta_ppn);		//find delta data in temp_buffer;
{
	UINT32 lpn, offset;
	UINT32 i;
	for(i = 0; i < META_COUNT; i++)
	{
		lpn = read_dram_32(TEMP_BUF_PTR(buf_num) + i * 2 * sizeof(UINT32));
		if(find_ppn(lpn) == delta_ppn)
			break;
	}

	if(i == META_COUNT)
	{
		return -1;
	}
	else
	{
		return (TEMP_BUFFER + i * 2 * sizeof(UINT32));
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

ftl_write()
{
	if(is_in_write_buffer())	//is in write buffer?
	{
		//버퍼 내용 수정 후 리턴
		//slru cache?
		return;
	}
	else
	{
		evict();		//write(not in write buffer)
		//write input data in write buffer
	}
}

evict()					//write(not in write buffer)
{
	if(is_in_cache())		//is in cache?
	{
		//find ppn in cache
		load_original_data();		//load original data
		if(in_protected_region())	//was ppn in slru protected region (before pop)
		{
			//XOR
			if(write_to_delta() != NULL)	//write to delta write buffer
			{
				;
			}
			else				//write to delta fail
			{
				get_free_page();	//get free page
				save_original_data();	//save original data
			}
		}
		else
		{
			get_free_page();		//get free page
			save_original_data();		//save original data
		}
		
		//pop and push in first slru(protected) slot
		
	}
	else				//not in cache
	{
		//find ppn in page and make cache node
		
		load_original_data();			//load original data
		get_free_page();			//get free page
		save_original_data();			//save original data

		//pop and push in first slru(probational) slot
	}
	
}

UINT32 write_to_delta(UINT32 delta_ppn)	//write to delta write buffer
{
	UINT32 inv_delta = -1;
	UINT32 cs;
	UINT32 delta_page;
	UINT32 bank;
	if((cs = _lzf_compress(TEMP_BUF(2), TEMP_BUF(1))) <= MAX_COMPRESS_SIZE)			//try lzf compress and compressed
	{
		if(is_remain_delta_write_buffer(cs))	//is remain delta_write_buffer?
		{
			inv_delta = find_delta_data(1, delta_ppn);
			if(inv_delta != -1)
			{
				write_dram_32(inv_delta, -1);	//invalid prev delta
			}
			put_delta();					//put compressed delta in delta write buffer
			return 0;
		}
		else								//not remain delta_write_buffer
		{
			delta_page = get_free_page();				//get free page
			save_delta_page(bank, delta_page);				//save delta page
			put_delta();					//put compressed delta in delta write buffer
			return 0;
		}
	}
	else								//not compressed
	{
		return -1;
	}
}
get_free_page()		//get free page
{
	
}
save_original_data()	//write as original data
{
	
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

UINT32 is_remain_delta_buffer(UINT32 cs);	//is remain in delta_buffer?
{
	if((next_delta_offset + cs) > PAGE_SIZE)
	{
		return 0;
	}
	if(next_delta_meta / (2 * sizeof(UINT32)) == META_COUNT)
	{
		return 0;
	}

	return 1;

}

save_delta_page(UINT32 bank, UINT32 delta_ppn)		//save delta page in flash
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

put_delta();			//put delta data in delta_write_buffer

