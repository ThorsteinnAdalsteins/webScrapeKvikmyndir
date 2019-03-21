fRun.init <- function(this.folder){
  rFiles <- list.files(path = this.folder,
                       pattern = '\\.r|\\.R',
                       full.names = TRUE)
  rFiles <- rFiles[-1] # fjarlægir __init__.R
  for(i in seq(length(rFiles))){
    source(rFiles[i], encoding = 'UTF8')
  }
}

fRun.init(this.folder = './R_Sources')
rm(fRun.init)
