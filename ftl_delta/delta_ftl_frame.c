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
have_delta();			//is the ppn has delta?
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
		
		if(have_delta())		//is the ppn has delta?
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

is_in_write_buffer();		//is in write buffer?
is_in_cache();			//is in cache?
load_original_data();		//load original data
have_delta();			//is the ppn has delta?
read_from_delta()		//read delta to temp2 buffer
{
	if(ppa_delta() == NULL)
	{
		copy_to_temp_buffer();	//copy from delta_write_buffer to temp_buffer (use temp1, temp2 buffer)
	}
	else
	{
		nand_page_read();	//load from nand to temp_buffer
	}
	
	find_delta_data();		//find delta data in temp_buffer;
	_lzf_decompress();		//decompress data
	return;
}

in_protected_region();		//was ppn in slru protected region (before pop)
copy_to_temp_buffer();		//copy from delta_write_buffer to temp_buffer (use temp1, temp2 buffer)
find_delta_data();		//find delta data in temp_buffer;
_lzf_decompress();		//decompress data





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

write_to_delta()	//write to delta write buffer
{
	if(_lzf_compress() != NULL)			//try lzf compress and compressed
	{
		if(is_remain_delta_write_buffer())	//is remain delta_write_buffer?
		{
			put_delta();					//put compressed delta in delta write buffer
			return 0;
		}
		else								//not remain delta_write_buffer
		{
			get_free_page();				//get free page
			save_delta_page();				//save delta page
			put_delta();					//put compressed delta in delta write buffer
			return 0;
		}
	}
	else								//not compressed
	{
		return -1;
	}
}
get_free_page();		//get free page
save_original_data();	//write as original data
_lzf_compress();	//compress by lzf
is_remain_delta_write_buffer();	//is remain in delta_write_buffer?
put_delta();			//put delta data in delta_write_buffer

