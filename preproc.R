
cit1 <- read.csv("apddb_x1.csv", sep=";", quote = "", 
                 row.names = NULL, 
                 stringsAsFactors = FALSE)
cit2 <- read.csv("apddb_x2.csv", sep=";", quote = "", 
                 row.names = NULL, header=TRUE,
                 stringsAsFactors = FALSE)
cit3 <- read.csv("apddb_x3.csv", sep=";", quote = "", 
                 row.names = NULL, header=TRUE,
                 stringsAsFactors = FALSE)

cit<-rbind(cit1,cit2,cit3)

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
idxs = grepl("Licencjat z zastosowañ fizyki",cit$field)
cit$field[idxs]="zfbm"
idxs = grepl("Licencjat z astronomii",cit$field)
cit$field[idxs]="ast"
idxs = grepl("Licencjat z in¿ynierii nano",cit$field)
cit$field[idxs]="in"
#master fields
idxs = grepl("Magisterium z fizyki",cit$field)
cit$field[idxs]="fiz"
idxs = grepl("Magisterium na kierunku fizyka",cit$field)
cit$field[idxs]="fiz"
idxs = grepl("Magisterium na kier. fizyka",cit$field)
cit$field[idxs]="fiz"
idxs = grepl("Magisterium z zastosowañ fizyki",cit$field)
cit$field[idxs]="zfbm"
idxs = grepl("Magisterium z astronomii",cit$field)
cit$field[idxs]="ast"
idxs = grepl("Magisterium z in¿ynierii nano",cit$field)
cit$field[idxs]="in"

cit<-cit[-which((nchar(cit$field)>4)),]
#cit<-cit[-which((nchar(cit$field)<2)),]
cit<-cit[-which(is.na(cit$field)),]

# name to gender
zx<-strsplit(cit$author," ")

zx[sapply(zx,length)<3]=TRUE
zx[sapply(zx,length)>2]=FALSE
cit$author(which(unlist(zx)))
cit$author<-gsub("mgr ", "", cit$author)
cit$author<-gsub("in¿. ", "", cit$author)

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
cit$data<-gsub("wrzeœnia", "wrz", cit$data)
cit$data<-gsub("paŸdziernika", "paŸ", cit$data)
cit$data<-gsub("listopada", "lis", cit$data)
cit$data<-gsub("grudnia", "gru", cit$data)
cit$data<-as.Date(cit$data,"%d %b %Y")

nazwy_kol=colnames(cit)
nazwy_kol[6]="gender"
colnames(cit)=nazwy_kol
save(cit,file="apd.Rda")

