fGet.blog.table <- function(url){
  try(
    url %>% 
      read_html() %>%
      html_node(xpath = '//*[@id="primary"]') %>% 
      html_node('table') %>% 
      html_table()
  )
} 