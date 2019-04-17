
# fyrst þarf að sækja upplýsingar um hvaða undirsíður eru til
rm(list = ls())
source('./R_Sources/__init__.R')


visited.sites <- read.csv2('./_GognUt/visited.sites.csv',
                           stringsAsFactors = FALSE)

urls <- c('http://frisk.klapptre.is/category/adsokn/',
          'http://frisk.klapptre.is/category/adsokn/page/2',
          'http://frisk.klapptre.is/category/adsokn/page/3',
          'http://frisk.klapptre.is/category/adsokn/page/4',
          'http://frisk.klapptre.is/category/adsokn/page/5',
          'http://frisk.klapptre.is/category/adsokn/page/6',
          'http://frisk.klapptre.is/category/adsokn/page/7',
          'http://frisk.klapptre.is/category/adsokn/page/8',
          'http://frisk.klapptre.is/category/adsokn/page/9')
  
# collects all paths to blog-sites        
sites <- lapply(urls, fGet.path.to.blogs)
sites <- unlist(sites)

new.visited.sites = tibble(
  site.url = sites,
  visited = Sys.time()
)

unvisited.sites <- new.visited.sites %>% filter(!(.$site.url %in% visited.sites$site.url))

if(nrow(unvisited.sites)>0){
  # sæki töflurnar sem vantar uppá
  new.table <- lapply(unvisited.sites$site.url, fGet.blog.table)
  
  # bæti gögnunum í listann
  visited.sites <- rbind(unvisited.sites, visited.sites)
  # skrifa nýju töflurnar inn í listann sem þarf
  write.csv2(x = visited.sites, file = './_GognUt/visited.sites.csv',
             row.names = FALSE)
}


# sæki töflurnar. 
# sækir allar töflurnar -- þetta getur tekið smá tíma
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
  

# geymi gögnin í dput skrá
dput(tables, './_GognUt/toflur.komnar.dput')


# fClean.pre.rbind er fall sem hreinsa ramman áður en hægt er að gera rbind
tables.list.clean <- lapply(tables, fClean.pre.rbind)

t.long <- do.call(rbind, tables.list.clean)
t.long$info <- str_replace(row.names(t.long), '\\.[:digit:]+', '')
t.long <- as_tibble(t.long)

# fClean.post.rbind extracts blog date and weekend date
# range from the info column
t.final <- fClean.post.rbind(t.long)

# write.csv2(x = t.final, './_GognUt/movieAttendanceInIceland.csv', row.names = FALSE, append = TRUE)

write.csv2(x = t.final, './_GognUt/movieAttendanceInIceland.csv', row.names = FALSE)


