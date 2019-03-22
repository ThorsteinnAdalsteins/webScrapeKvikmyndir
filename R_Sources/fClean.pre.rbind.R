# cleaning routine to unify frames
# prior to rbind into one frame
fClean.pre.rbind <- function(df){
  
  if(all(names(df)[1:3] != c("This\n Week", "Last\n Week", "Film Title"))){
    dnames <- df[1,]
    df <- df[-1, ]
    names(df) <- dnames
  }
  
  names(df) <- names(df) %>% str_replace('\n', '') %>% str_replace_all(' ', '.') %>% 
    str_replace_all('\\.\\.', '.') %>% tolower()
  
  ds <- df %>% select("this.week", 
                      "last.week", 
                      "film.title", 
                      "distributor.name", 
                      "wks.inrelease", 
                      "gross.b.o.we", 
                      "adm.we", 
                      "adm.to.date",
                      "total.b.o.to.date")
  
  df.out <- ds %>% mutate(this.week = as.integer(this.week),
                          last.week = as.integer(last.week),
                          wks.inrelease = as.integer(wks.inrelease),
                          gross.b.o.we = str_replace_all(.$gross.b.o.we, '\\.|,', '') %>% 
                            as.numeric(),
                          adm.we = str_replace_all(.$adm.we, '\\.|,', '') %>% 
                            as.numeric(),
                          adm.to.date = str_replace_all(.$adm.to.date, '\\.|,', '') %>% 
                            as.numeric(),
                          total.b.o.to.date = str_replace_all(.$total.b.o.to.date, '\\.|,', '') %>% 
                            as.numeric()) %>%
    filter(!is.na(this.week)) %>%
    as_tibble()
  
  return(df.out)
}
