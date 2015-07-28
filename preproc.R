
cit1 <- read.csv("apddb_x1.csv", sep=";", quote = "", 
                 row.names = NULL, 
                 stringsAsFactors = FALSE)
cit2 <- read.csv("apddb_x2.csv", sep=";", quote = "", 
                 row.names = NULL, header=TRUE,
                 stringsAsFactors = FALSE)
cit3 <- read.csv("apddb_x3.csv", sep=";", quote = "", 
                 row.names = NULL, header=TRUE,
                 stringsAsFactors = FALSE)
cit4 <- read.csv("apddb_x4.csv", sep=";", quote = "", 
                 row.names = NULL, header=TRUE,
                 stringsAsFactors = FALSE)
cit4<-cit4[,1:10]
cit<-rbind(cit1,cit2,cit3,cit4)

cit[cit=='brak']=NA
cit[cit=='(brak)']=NA
cit[cit=='(brak informacji)']=NA
cit$degree[cit$degree=="Licencjat"]=1
cit$degree[cit$degree=="Magisterium"]=2
cit<-transform(cit, degree=as.numeric(degree))

cit$lang[cit$lang=="polski [PL]"]="pl"
cit$lang[cit$lang=="angielski [EN]"]="en"

#bachelor fields
idxs = grepl("Licencjat z fizyki",cit$field)
cit$field[idxs]="fiz"
idxs = grepl("Licencjat z zastosowań fizyki",cit$field)
cit$field[idxs]="zfbm"
idxs = grepl("Licencjat z astronomii",cit$field)
cit$field[idxs]="ast"
idxs = grepl("Licencjat z inżynierii nano",cit$field)
cit$field[idxs]="in"
#master fields
idxs = grepl("Magisterium z fizyki",cit$field)
cit$field[idxs]="fiz"
idxs = grepl("Magisterium na kierunku fizyka",cit$field)
cit$field[idxs]="fiz"
idxs = grepl("Magisterium na kier. fizyka",cit$field)
cit$field[idxs]="fiz"
idxs = grepl("Magisterium z zastosowań fizyki",cit$field)
cit$field[idxs]="zfbm"
idxs = grepl("Magisterium z astronomii",cit$field)
cit$field[idxs]="ast"
idxs = grepl("Magisterium z inżynierii nano",cit$field)
cit$field[idxs]="in"

cit<-cit[-which((nchar(cit$field)>4)),]
#cit<-cit[-which((nchar(cit$field)<2)),]
cit<-cit[-which(is.na(cit$field)),]

# name to gender
zx<-strsplit(cit$author," ")

cit$author<-gsub("mgr ", "", cit$author)
cit$author<-gsub("inż. ", "", cit$author)

df <- data.frame(matrix(unlist(strsplit(cit$author," ")), nrow=dim(cit)[1], byrow=T),stringsAsFactors=FALSE)
cit$author[grep("a$",df$X1)]="F"
cit$author[-grep("a$",df$X1)]="M"

# date string to date format
cit$data<-gsub("stycznia", "sty", cit$data)
cit$data<-gsub("lutego", "lut", cit$data)
cit$data<-gsub("marca", "mar", cit$data)
cit$data<-gsub("kwietnia", "kwi", cit$data)
cit$data<-gsub("maja", "maj", cit$data)
cit$data<-gsub("czerwca", "cze", cit$data)
cit$data<-gsub("lipca", "lip", cit$data)
cit$data<-gsub("września", "wrz", cit$data)
cit$data<-gsub("października", "paź", cit$data)
cit$data<-gsub("listopada", "lis", cit$data)
cit$data<-gsub("grudnia", "gru", cit$data)
cit$data<-as.Date(cit$data,"%d %b %Y")

nazwy.kol=colnames(cit)
nazwy.kol[6]="gender"
colnames(cit)=nazwy.kol
data.idcs <-cit$data>"2009-01-01"
data.idcs[is.na(data.idcs)] <- TRUE
apddata <- cit[data.idcs,]

save(apddata,file="apd.Rda")
