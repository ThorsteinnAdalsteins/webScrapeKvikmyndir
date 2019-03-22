
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

tables.list.clean <- lapply(tables, fClean.table)


t.long <- do.call(rbind, tables.list.clean)
t.long$info <- str_replace(row.names(t.long), '\\.[:digit:]+', '')
t.long <- as_tibble(t.long)

tx <- t.long %>% tidyr::separate(col = info, into = c('blog.y', 'blog.m', 'blog.d', 'rest', 'scr'), sep = '/') 

tx$rest <- str_replace_all(tx$rest ,'til', '$')
tx$fra.dag <- str_extract(tx$rest, '[:digit:]+')
tx$rest <- str_replace(tx$rest, coalesce(tx$fra.dag, 'NONE'), '$')
tx$til.dag <- str_extract(tx$rest, '[:digit:]+')
tx$rest <- str_replace(tx$rest, coalesce(tx$til.dag, 'NONE'), '$')
tx$fra.ar <- str_extract(tx$rest, '[:digit:][:digit:][:digit:][:digit:]')
tx$rest <- str_replace(tx$rest, coalesce(tx$fra.ar, 'NONE'), '$')
tx$fra.man <- str_extract(tx$rest, '[:alpha:]+')
tx$rest <- str_replace(tx$rest, coalesce(tx$fra.man, 'NONE'), '$')
tx$til.man <- str_extract(tx$rest, '[:alpha:]+')
tx$rest <- str_replace(tx$rest, coalesce(tx$til.man, 'NONE'), '$')

# laga null
tx$fra.ar <- coalesce(tx$fra.ar, tx$blog.y)
tx$til.man <- coalesce(tx$til.man, tx$fra.man)
tx$til.ar <- tx$fra.ar


# reyni að laga mánuðina og sækja númer fyrir mánuði
minimal.month <- tibble(
  m = c('jan', 'feb', 'mar', 
        'apr', 'mai', 'may', 
        'jun', 'jul', 'agu', 
        'aug', 'sep', 'okt', 
        'oct', 'nov', 'des', 
        'dec'),  
  n = c(1, 2, 3, 
        4, 5, 5, 
        6, 7, 8, 
        8, 9, 10, 
        10, 11, 12, 
        12)
)

dx <- tx %>% mutate(
  fra.man.fyrstu3 = str_sub(.$fra.man, end = 3),
  til.man.fyrstu3 = str_sub(.$til.man, end = 3)
)

dx <- dx %>% 
  left_join(minimal.month, by = c('fra.man.fyrstu3' = 'm')) %>%
  left_join(minimal.month, by = c('til.man.fyrstu3' = 'm'))

dx <- dx %>% mutate(
  man.fra.n = coalesce(n.x, as.numeric(blog.m)),
  man.til.n = coalesce(n.y, as.numeric(blog.m))
) 

out.d <- dx %>% mutate(blog.date = ymd(str_c(blog.y, blog.m, blog.d, sep = '/')),
                       weekend.start = ymd(str_c(fra.ar, man.fra.n, fra.dag, sep ='/')),
                       weekend.end = ymd(str_c(til.ar, man.til.n, til.dag, sep = '/'))) %>%
  select(blog.date, 
        rank.this.week = this.week, 
        rank.last.week = last.week, 
        weeks.in.release = wks.inrelease,
        
        film.title,
        distributor.name, 
        
        gross.box.o.weekend = gross.b.o.we, 
        adm.weekend = adm.we, 
        weekend.start, weekend.end,
        
        adm.to.date,
        total.box.o.to.date = total.b.o.to.date)

view(out.d)
  