fGet.blog.table <- function(url){
  tryCatch(
    {
      page <- url %>% 
        read_html() %>%
        html_node(xpath = '//*[@id="primary"]') %>% 
        html_node('table') %>% 
        html_table()
      
      page$url <- url
      
      return(page)
    },
    error = function(cond) {
      message(paste("URL does not seem to exist:", url))
      message("Here's the original error message:")
      message(cond)
      return(NA)
    } 
  )
} 
