hobo_import <- function(file) {
  # file <- paste0(here, "/scan-tree/data/test1.fp")
  # file <- paste0(here,
  #                "/data/hobo/pressure_corrected/SF-A/hobo.10571087_SF-A_180719_180907.csv")
  
  #record file/sample ID
  basename <- str_sub(basename(file), 1, -5)
  
  #Get metadata in first row 
  id1 <- read_csv(file, skip=0, n_max = 1, col_names = FALSE) %>% pull(1)

  #read in rest of data - test change
  dat <- read_csv(file, skip=1)
  dat2 <- dat %>% 
    select(2, 4, 6) %>% 
    drop_na()
  
  colnames(dat2) <- c("dt_GMTm5", "T_C", "depth_m")
    
  #combine
  dat3 <- dat2 %>% 
    mutate(id1 = id1,
           file = basename,
           dt = mdy_hms(dt_GMTm5, tz = "EST")) %>% 
    select(file, id1, dt, everything())
    
  return(dat3)
}


