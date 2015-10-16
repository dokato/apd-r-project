library("ggplot2")
library("dplyr")

load("apd.Rda")
# changing date to a year only
apddata$rok <- as.numeric(format(apddata$data,"%Y"))

# cleaning rows with year NA
apddatacl <- apddata[!is.na(apddata$rok),]

cbbPalette <- c("#ff8c19", "#baff1e")

ggplot(apddatacl, aes(x=factor(apddatacl$rok))) + 
  geom_bar(aes(fill=lang), binwidth=1, width=0.7) +
  scale_fill_manual("Kod", values=cbbPalette, labels = c("angielski", "polski")) +
  xlab("rok") + ylab("liczba prac") + 
  theme(legend.position="right") +
  ggtitle("Język prac dyplomowych na FUW") +
  theme(plot.title=element_text(face="bold", size=16))


