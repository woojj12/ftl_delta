ftl_read()
{
  if(is_in_write_buffer())	//is in write buffer?
	{
		//버퍼 내용 리턴
		return;
	}
	if(is_in_cache())			//is in cache?
	{
		//find ppn in cache
		//pop and push in first slru(protected) slot
		load_original_data();	//load original data
		
		if(have_delta())		//is the ppn has delta?
		{
			read_from_delta();	//read delta
			//XOR operation
			if(in_protected_region())	//was ppn in slru protected region (before pop)
			{
				merge();		//write merge data
				return;
			}
			else
			{
				return;
			}
		}
		else
		{
			return;
		}
	}
	else						//not in cache
	{
		//find ppn in page and make cache node
		//pop and push in first slru(probational) slot
		load_original_data();	//load original data
	}
}

ftl_write()
{
	if(is_in_write_buffer())	//is in write buffer?
	{
		//버퍼 내용 수정 후 리턴
		return;
	}
	else
	{
		evict();		//write(not in write buffer)
		//write input data in write buffer
	}
}

//function list
//read stream function
is_in_write_buffer();	//is in write buffer?
is_in_cache();			//is in cache?
load_original_data();	//load original data
have_delta();			//is the ppn has delta?
read_from_delta();		//read delta
in_protected_region();	//was ppn in slru protected region (before pop)

//write stream function (not in read stream function)
evict();				//write(not in write buffer)
write_to_delta();		//write to delta write buffer
get_free_page();		//get free page
save_original_data();	//write as original data


//functions
//read stream function
is_in_write_buffer();	//is in write buffer?
is_in_cache();			//is in cache?
load_original_data();	//load original data
have_delta();			//is the ppn has delta?
read_from_delta()		//read delta

in_protected_region();	//was ppn in slru protected region (before pop)

//write stream function (not in read stream function)
evict()					//write(not in write buffer)
{
	if(is_in_cache())		//is in cache?
	{
		//find ppn in cache
		//pop and push in first slru(protected) slot
		load_original_data();		//load original data
		if(in_protected_region())	//was ppn in slru protected region (before pop)
		{
			//XOR
			if(write_to_delta() != NULL)	//write to delta write buffer
			{
				return;
			}
			else						//write to delta fail
			{
				get_free_page();		//get free page
				save_original_data();	//save original data
				return;
			}
		}
		else
		{
			get_free_page();			//get free page
			save_original_data();		//save original data
		}
	}
	else				//not in cache
	{
		//find ppn in page and make cache node
		//pop and push in first slru(probational) slot
		load_original_data();		//load original data
		get_free_page();			//get free page
		save_original_data();		//save original data

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
