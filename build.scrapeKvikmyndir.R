
# fyrst þarf að sækja upplýsingar um hvaða undirsíður eru til

source('./R_Sources/__init__.R')
library(lubridate)

visited.sites <- read.csv2('./_GognUt/visited.sites.csv',
                           stringsAsFactors = FALSE)


url <- 'http://frisk.klapptre.is/category/adsokn/page/9/'

sites <- fGet.path.to.blogs(url)

new.visited.sites = tibble(
  site.url = sites,
  visited = Sys.time()
)

unvisited.sites <- new.visited.sites %>% filter(!(.$site.url %in% visited.sites$site.url))

if(nrow(unvisited.sites)>0){
  # sæki töflurnar sem vantar uppá
  new.table <- lapply(unvisited.sites$site.url, fGet.blog.table)
  
  # geymi gögnin í dput skrá
  dput(tables, './_GognUt/toflur.komnar.dput')
  
  # bæti gögnunum í listann
  visited.sites <- rbind(unvisited.sites, visited.sites)
  # skrifa nýju töflurnar inn í listann sem þarf
  write.csv2(x = visited.sites, file = './_GognUt/visited.sites.csv',
             row.names = FALSE)
}

# sæki töflurnar
tables <- lapply(visited.sites$site.url, fGet.blog.table)

names(tables) <- str_replace(visited.sites$site.url, 'http://frisk.klapptre.is/', '') %>%
  str_replace('tekjur-', '') %>%
  str_replace('adsokn-', '') %>%
  str_replace('helgina-', '') %>%
  str_replace('og-', '') %>%
  str_replace('tekur-', '') %>%
  str_replace('i-', '') %>%
  str_replace('kvikmyndahus-', '') %>%
  str_replace('kvikmyndahusum-', '') %>%
  str_replace('kvikmyndahusa-', '')
  
  

## þarf að búa til "clean" fall

fClean.table <- function(d){
  
  if(all(names(d)[1:3] != c("This\n Week", "Last\n Week", "Film Title"))){
    dnames <- d[1,]
    d <- d[-1, ]
    names(d) <- dnames
  }
  
  names(d) <- names(d) %>% str_replace('\n', '') %>% str_replace_all(' ', '.') %>% 
    str_replace_all('\\.\\.', '.') %>% tolower()
  
  ds <- d %>% select("this.week", 
                     "last.week", 
                     "film.title", 
                     "distributor.name", 
                     "wks.inrelease", 
                     "gross.b.o.we", 
                     "adm.we", 
                     "adm.to.date",
                     "total.b.o.to.date")
  
  df <- ds %>% mutate(this.week = as.integer(this.week),
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
  
  return(df)
}

dc <- lapply(tables, fClean.table)


dcb <- do.call(rbind,dc)
dcb$info <- str_replace(row.names(dcb), '\\.[:digit:]+', '')
dcb <- as_tibble(dcb)

dcbd <- dcb %>% tidyr::separate(col = info, into = c('blog.y', 'blog.m', 'blog.d', 'rest', 'scr'), sep = '/') 

# þarf að ná sýningardögunum ut úr vefslóðinni
# þetta er svolítið clusterfudge
fSplit.rest <- function(rest.vec){
  
  rest <- rest.vec
  rest1 <- str_replace_all(rest ,'til', '$')
  fra.dag <- str_extract(rest1, '[:digit:]+')
  rest2 <- str_replace(rest1, fra.dag, '$')
  til.dag <- str_extract(rest2, '[:digit:]+')
  rest3 <- str_replace(rest2, til.dag, '$')
  fra.ar <- str_extract(rest3, '[:digit:][:digit:][:digit:][:digit:]')
  rest4 <- str_replace(rest3, coalesce(fra.ar, '2000'), '$')
  fra.man <- str_extract(rest4, '[:alpha:]+')
  rest5 <- str_replace(rest4, fra.man, '$')
  til.man <- str_extract(rest5, '[:alpha:]+')
  out.data <- tibble(rest, fra.dag, fra.man, til.dag, til.man, fra.ar)

  return(out.data)
}

dcdb <- dcbd %>% dplyr::inner_join(fSplit.rest(dcbd$rest))
# laga null
dcdb$fra.ar <- coalesce(dcdb$fra.ar, dcdb$blog.y)
dcdb$til.man <- coalesce(dcdb$til.man, dcdb$fra.man)
dcdb$til.ar <- dcdb$fra.ar

view(dcdb)

# reyni að laga mánuðina og sækja 
minimal.month <- tibble(
  m = c('jan', 'feb', 'mar', 'apr', 'mai', 'may', 'jun', 'jul', 'agu', 'aug', 'sep', 'okt', 'oct', 'nov', 'des', 'dec'),  
  n = c(1, 2, 3, 4, 5, 5, 6, 7, 8, 8, 9, 10, 10, 11, 12, 12)
)

dx <- dcdb %>% mutate(
  fra.man.fyrstu3 = str_sub(.$fra.man, end = 3),
  til.man.fyrstu3 = str_sub(.$til.man, end = 3)
)


dx <- dx %>% 
  left_join(minimal.month, by = c('fra.man.fyrstu3' = 'm')) %>%
  left_join(minimal.month, by = c('til.man.fyrstu3' = 'm'))


view(dx)
