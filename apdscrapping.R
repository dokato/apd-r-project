# APD WEB SCRAPING
# by dokato (11-07-2015)

library(rvest)
library(stringr)

trim <- function(x) gsub("^\\s+|^\n|\\s+$", "", x)

infopageapd <-function(url){
  diploma <- html(url)
  tinfo<-diploma %>% 
    html_nodes("#thesisInfo tr:nth-child(1) td") %>%
    html_text()
  tit<-diploma %>% 
    html_nodes("#thesisInfo .width-100 .width-100") %>%
    html_text()
  degr<-diploma %>%
    html_nodes("#thesisInfo div:nth-child(1)") %>%
    html_text()
  dat<-diploma %>%
    html_nodes("tr:nth-child(8) td") %>%
    html_text()
  egzdat<-diploma %>%
    html_nodes("#thesisInfo div:nth-child(2)") %>%
    html_text()
  
    tit<-trim(tit)
    dat<-trim(dat)
    tinfo<-trim(tinfo)
    egzdat<-trim(unlist(strsplit(trim(egzdat), "\n   ")))
    field<-trim(degr)
    degr<-trim(unlist(strsplit(trim(degr), " ")))[1]
    
    
    title.pl <- tit[1]
    title.eng <- tit[2]
    if (is.na(tit[5])){ # old diploma webpage formatting style
      key.pl <- tit[4]
      key.eng <- NA 
      if (tit[1]==tit[2]){
        lang <- "angielski [EN]"
      }
      else{
        lang <- "polski [PL]"
      }
    }
    else{ # new diploma webpage formatting style
      key.pl <- tit[5]
      key.eng <- tit[6]
      lang <-tinfo[2]
    }
    author <- tinfo[5]
    prom <- tinfo[7]
    data<- egzdat[4]
    
  return(c(title.pl, title.eng, key.pl, key.eng, lang, author, prom, data, degr, field))
}

# from catalogue

catu1<-"https://apd.uw.edu.pl/catalogue/browse/diploma/?page="
catu2<-"&order=-delivered_date"
catu3<-"https://apd.uw.edu.pl"

####################### MAIN LOOP
apddb <- c("titpl", "titeng", "keypl", "keyeng", "lang", "author", "promotor","data", "degree", "field")
for (i in 3001:4000){
  cat.page <- paste(catu1, toString(i),catu2, sep="")
  web <- html(cat.page)
  links<-web %>% html_nodes("a") %>% html_attr("href")
  links<-links[grepl("diplomas",links)] 
  links<-links[2:length(links)]
  instit<-web %>% html_nodes("small a") %>% html_text()
  fuw <- grep("11000000",instit)
  if (length(fuw)==0){
    next
  }
  else{
    cat(i, " ")
    for (k in fuw){
       pdurl<-paste(catu3, links[k], sep="")
       res<-infopageapd(pdurl)
       apddb <- rbind(apddb, res, deparse.level=0)
    }
  }
}

write.table(apddb,"apddb_x4.csv", sep=";", quote=FALSE, row.names=FALSE, col.names=FALSE)
#######################
