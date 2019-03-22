
# fClean.post.rbind extracts blog date and weekend date
# range from the info column
fClean.post.rbind <- function(df){
  
  tx <- df %>% tidyr::separate(col = info, 
                               into = c('blog.y', 'blog.m', 'blog.d', 'rest', 'scrrap'), 
                               sep = '/') 
  
  # the URL has blog date an info about the weekend period
  # not a pretty thing, but works
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
  
}