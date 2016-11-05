# APD WEB SCRAPING
# by dokato (11-07-2015)
library('xml2')

library(rvest)
library(stringr)

trim <- function(x) gsub("^\\s+|^\n|\\s+$", "", x)

infopageapd <-function(url){
  diploma <- read_html(url)
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

newinfopageapd <-function(url){
  # some amendments added to new version because webpage has changed

  diploma <- read_html(url)
  tit<-diploma %>% 
    html_nodes("#thesisInfo .width-100 .width-100") %>%
    html_text()
  tit <- tit[grepl("^[A-Za-z]+",tit)]
  tit <- ifelse(length(tit)>2, tit[0], tit)
  
  tit.eng<-diploma %>% 
    html_nodes("tr:nth-child(2) .tr:nth-child(2) .width-100") %>%
    html_text()
  tit.eng<-trim(tit.eng)
  tit.eng <- tit.eng[grepl("^[A-Za-z]+",tit.eng)]
  tit.eng <- ifelse(length(tit.eng)>2, tit.eng[0], tit.eng)

  lang<-diploma %>% 
    html_nodes("tr:nth-child(1) td:nth-child(2)") %>%
    html_text()
  lang <- trim(lang)

  auth <-diploma %>% 
    html_nodes("#thesisInfo .width-100 a") %>%
    html_text()
  auth <- auth[grepl("^[A-Za-z]+", auth)]
  auth <- ifelse(length(auth)>2, auth[0], auth)
  
  dat<-diploma %>%
    html_nodes(".note div:nth-child(2)") %>%
    html_text()
  dat<-trim(unlist(strsplit(trim(dat), "\n   ")))
  dat<-dat[length(dat)]

  field<-diploma %>%
    html_nodes(".note div:nth-child(1)") %>%
    html_text()

  field<-trim(field)

  prom<-diploma %>%
    html_nodes("tr:nth-child(4) a") %>%
    html_text()
  prom<-trim(prom)
  if (length(prom)>1){
    prom = prom[0]
  }
  
  key.pl <- diploma %>%
    html_nodes("tr:nth-child(11) .tr:nth-child(1) .width-100") %>%
    html_text()
  key.pl <- trim(key.pl)
  key.eng <- diploma %>%
    html_nodes("tr:nth-child(11) .tr:nth-child(2) .width-100") %>%
    html_text()
  key.eng <- trim(key.eng)

  if (grepl('\\[EN\\]',lang)){
    tmpt <- tit.eng
    tit.eng <- tit
    tit <- tmpt
    tmpk <- key.eng
    key.eng <- key.pl
    key.pl <- tmpk
  }
  tit<-trim(tit)
  tit.eng<-trim(tit.eng)
  auth<-trim(auth)
  
  dat<-trim(dat)

  degr<-trim(unlist(strsplit(field, " ")))[1]

  return(c(tit, tit.eng, key.pl, key.eng, lang, auth, prom, dat, degr, field))
  
}
# from catalogue

catu1<-"https://apd.uw.edu.pl/catalogue/browse/diploma/?page="
catu2<-"&order=-delivered_date"
catu3<-"https://apd.uw.edu.pl"

####################### MAIN LOOP
apddb <- c("titpl", "titeng", "keypl", "keyeng", "lang", "author", "promotor","data", "degree", "field")
for (i in 81:120){
  cat.page <- paste(catu1, toString(i),catu2, sep="")
  web <- read_html(cat.page)
  links<-web %>% html_nodes("a") %>% html_attr("href")
  links<-links[grepl("diplomas",links)] 

  instit<-web %>% html_nodes("a:nth-child(3)") %>% html_text()
  instit<-instit[grepl("\\[.*\\]",instit)]
  fuw <- grep("11000000",instit)
  if (length(fuw)==0){
    next
  }
  else{
    cat(i, " ")
    for (k in fuw){
       pdurl<-paste(catu3, links[k], sep="")
       res<-newinfopageapd(pdurl)
       apddb <- rbind(apddb, res, deparse.level=0)
    }
  }
}

write.table(apddb,"apddb_x0.csv", sep=";", quote=FALSE, row.names=FALSE, col.names=FALSE)
#######################
