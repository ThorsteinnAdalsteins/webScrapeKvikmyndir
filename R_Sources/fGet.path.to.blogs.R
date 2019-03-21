fGet.path.to.blogs <- function(url){
  
  page <- read_html(url)
  
  mainB <- page %>%
    html_node(xpath = '//*[@id="primary"]')
  
  post.ids <- mainB %>% 
    xml_child() %>% 
    html_nodes('article') %>% 
    xml2::xml_attr('id')
  
  extract.html.path <- function(x){
    mainB %>% 
      xml_child(1) %>% 
      xml_child(x) %>% 
      xml_child(1) %>% 
      xml_child(1) %>%
      xml_child(2) %>% 
      xml_child(1) %>% 
      xml_child(1) %>%
      html_attr('href')
  }
  
  html.paths <- character(length(post.ids))
  i <- 0
  for(i in seq(length(post.ids))){
    html.paths[i] <- extract.html.path(i)
  }
  
  return(html.paths)
}