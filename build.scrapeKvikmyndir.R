
# fyrst þarf að sækja upplýsingar um hvaða undirsíður eru til

source('./R_Sources/__init__.R')

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

d <- tables[[4]]
head(d)

if(all(names(d)[1:3] != c("This\n Week", "Last\n Week", "Film Title"))){
  dnames <- d[1,]
  d <- d[-1, ]
  names(d) <- dnames
}

names(d) <- names(d) %>% str_replace('\n', '') %>% str_replace_all(' ', '.') %>% 
  str_replace_all('\\.\\.', '.') %>% tolower()

