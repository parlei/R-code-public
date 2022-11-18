# Average of Normals för några analyser
#
# För analyserna är referensintervallen insatta för ålder och kön
# För Takrolimus och Ciklosporin saknas referensintervall, men det ritas
# då även ut en alternativ graf med höga outliers borttagna (övre gräns:
# Q3+1,5*IQR, undre Q1-1,5*IQR). För analyser där Equalis externa
# frekvent är extrema och det är få prover tas resultat med reserv-LID
# bort.
# Pär Leijonhufvud/2022-11-02

library(dplyr)
library(ggplot2)
library(ggrepel)
library(ggthemes)
library(reshape2)
library(stringr)
library(grid)
library(zoo)
library(dplyr)
library(tidyverse)

library(lubridate)
library(gridExtra)

### Parameters ###

# Bara ett år tillbaka i tiden
Start_Date <- as.Date(Sys.Date() -365)

# Spara bilder med suffix som inkluderar datatum
ExportFileSuffix <- paste0("-regelbunden_",Sys.Date(),".png")

# moving average funktion
ma <- function(x, n = ma.n){stats::filter(x, rep(1 / n, n), sides = 2)}

# Exportfil från QlikView, sparas med samma filnamn varje gång
aondata <- read.csv2("aon_vanliga.csv", fileEncoding = "UTF-8-BOM")

# Ta bort rader utan LID och färdigt resultat
aondata <- subset(aondata, !is.na(LIDN))
aondata <- subset(aondata, Tekniskt.godkännande !="")


# fixa datum, vecka, år: allt används inte just nu, men...
aondata$datum <- as.Date(aondata$Registreringstid, "%Y-%m-%d")
aondata$year <- format(as.Date(aondata$datum, format="%Y-%m-%d"),"%Y")
aondata$week <- strftime(aondata$datum, format="%V")
# aondata$Datum <- strptime(aondata$Tekniskt.godkännande, format ="%Y-%m-%d %H:%M", tz="GMT")
aondata$Datum <- as.POSIXct(aondata$Tekniskt.godkännande, na.rm=TRUE)

# Utan datum tas bort
aondata <- filter(aondata, !is.na(datum))

# Bara senaste året
aondata <- filter(aondata, datum > Start_Date)

# Behövs för referensintervall, kommer över som sträng med komma
aondata$Ålder <- as.numeric(sub(",",".",aondata$Ålder..År.))

## Skulle kunna köras med en funktion som tar en massa argument snarare
## än en separat snutt för varje analys. Men vissa av åldersintervallen
## är struliga, så det blir enklare så här

## Li

# Sätt intervall, filtrera ut de aktuella raderna, osv
analys <- "S-Litium"
print(analys)
ExportFile <- paste0("AoN_Litium", ExportFileSuffix)
norm.max <- 0.8
norm.min <- 0.3

litium <- filter(aondata, Analys==analys & 
		 Mätvärde >= norm.min & 
		 Mätvärde <= norm.max)


antal <- nrow(litium)
ma.N <- round(antal/52, 0)
medelvärde <- round(mean(litium$Mätvärde, na.rm=TRUE), 1)
antal <- nrow(litium)


caption.text <- paste0("AoN, fönster=", ma.N, ", referensintervall: ",
			     norm.min, "-",norm.max, ", medelvärde: ",
				round(mean(litium$Mätvärde),1) )
subtitle.text <- paste0("Totalt ", antal, " resultat")

p.litium <- ggplot(litium, aes(x=datum, y=Mätvärde))+ 
 	  geom_jitter(color="grey55", alpha=1/2)+ 
  	  geom_line(aes(y=rollmean(Mätvärde, ma.N, na.pad=TRUE , col="red"))) + 
 	  coord_cartesian(ylim = c(0, 1.5*norm.max)) +
  	 annotate("text", y=mean(litium$Mätvärde), x=as.Date(Start_Date), vjust=1, label=round(mean(litium$Mätvärde),1))+
 		 labs(title=analys, 
 		 caption=caption.text,
 		 subtitle=subtitle.text )+
 		 xlab("Datum ")+ 
	  scale_x_date(date_breaks = "1 month", date_labels =  "%Y %b")+
 	  geom_hline(yintercept = mean(litium$Mätvärde), col="blue", lty=2)+
 	  theme_minimal() +
	  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1))
 
ggsave(ExportFile, dpi=600)




# Bilirubin


# Sätt intervall, filtrera ut de aktuella raderna, osv
analys <- "S-Bilirubin"
ExportFile <- paste0("AoN_Bilirubin", ExportFileSuffix)
print(analys)
norm.min.1 <-  0
norm.max.1 <- 140
norm.min.2 <-  0
norm.max.2 <- 210
norm.min.3 <-  0
norm.max.3 <- 25
norm.min.4 <-  5
norm.max.4 <- 25

bilirubin <- filter(aondata, Analys==analys & 
			  ( # 0-2 dagar
		 Mätvärde >= norm.min.1 & 
		 Mätvärde <= norm.max.1 &
		 Ålder >0 & Ålder < 0.0055
     )
	  |    # 2-3 dagar
		 (Mätvärde >= norm.min.2 & 
		 Mätvärde <= norm.max.2 &
		     Ålder >=0.0055 & Ålder <0.0083 
		 ) 
	  |    # 3-7 dagar
		 (Mätvärde >= norm.min.3 & 
		 Mätvärde <= norm.max.3 &
		     Ålder >=0.0083 & Ålder <=0.0192 
		 ) 
	  |    # >7 dagar
		 (Mätvärde >= norm.min.3 & 
		 Mätvärde <= norm.max.3 &
		     Ålder >=0.0192 
		 ) 
)

bilirubin <- filter(bilirubin,  datum >= Start_Date) 

antal <- nrow(bilirubin)
ma.N <- round(antal/52, 0)
medelvärde <- round(mean(bilirubin$Mätvärde, na.rm=TRUE), 1)


caption.text <- paste0("AoN, fönster=", ma.N, ", 
			     referensintervall: 5-25 (vuxna)",
			      ", medelvärde: ",
				round(mean(bilirubin$Mätvärde),1) )
subtitle.text <- paste0("Totalt ", antal, " resultat")

p.bilirubin <- ggplot(bilirubin, aes(x=datum, y=Mätvärde))+ 
 	  geom_jitter(color="grey55", alpha=1/2)+ 
  	  geom_line(aes(y=rollmean(Mätvärde, ma.N, na.pad=TRUE , col="red"))) + 
  	 annotate("text", y=mean(bilirubin$Mätvärde), x=as.Date(Start_Date), vjust=1, label=round(mean(bilirubin$Mätvärde),1))+
 		 labs(title=analys, 
 		 caption=caption.text,
 		 subtitle=subtitle.text )+
 		 xlab("Datum ")+ 
	  scale_x_date(date_breaks = "1 month", date_labels =  "%Y %b")+
 	  geom_hline(yintercept = mean(bilirubin$Mätvärde), col="blue", lty=2)+
 	  theme_minimal() +
	  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1))
 
ggsave(ExportFile, dpi=600)




# Bilirubin, konjugerat


# Sätt intervall, filtrera ut de aktuella raderna, osv
analys <- "S-Bilirubin, konj"
print(analys)
ExportFile <- paste0("AoN_BilKonj", ExportFileSuffix)
norm.max <- 6
norm.min <-  0

bilkonj <- filter(aondata, Analys==analys & 
		 Mätvärde >= norm.min & 
		 Mätvärde <= norm.max
)

antal <- nrow(bilkonj)
ma.N <- round(antal/52, 0)
medelvärde <- round(mean(bilkonj$Mätvärde, na.rm=TRUE), 1)
antal <- nrow(bilkonj)


caption.text <- paste0("AoN, fönster=", ma.N, ", referensintervall: ",
			     norm.min, "-",norm.max, ", medelvärde: ",
				round(mean(bilkonj$Mätvärde),1) )
subtitle.text <- paste0("Totalt ", antal, " resultat")

p.bilkonj <- ggplot(bilkonj, aes(x=datum, y=Mätvärde))+ 
 	  geom_jitter(color="grey55", alpha=1/2)+ 
  	  geom_line(aes(y=rollmean(Mätvärde, ma.N, na.pad=TRUE , col="red"))) + 
 	  coord_cartesian(ylim = c(0, 1.5*norm.max)) +
  	 annotate("text", y=mean(bilkonj$Mätvärde), x=as.Date(Start_Date), vjust=1, label=round(mean(bilkonj$Mätvärde),1))+
 		 labs(title=analys, 
 		 caption=caption.text,
 		 subtitle=subtitle.text )+
 		 xlab("Datum ")+ 
	  scale_x_date(date_breaks = "1 month", date_labels =  "%Y %b")+
 	  geom_hline(yintercept = mean(bilkonj$Mätvärde), col="blue", lty=2)+
 	  theme_minimal() +
	  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1))
 
ggsave(ExportFile, dpi=600)






# VitaminD


# Sätt intervall, filtrera ut de aktuella raderna, osv
analys <- "S-VitaminD"
print(analys)
ExportFile <- paste0("AoN_VitaminD", ExportFileSuffix)
norm.max <- 250
norm.min <-  25

VitaminD <- filter(aondata, Analys==analys & 
		 Mätvärde >= norm.min & 
		 Mätvärde <= norm.max 
     )

antal <- nrow(VitaminD)
ma.N <- round(antal/52, 0)
medelvärde <- round(mean(VitaminD$Mätvärde, na.rm=TRUE), 1)
antal <- nrow(VitaminD)


caption.text <- paste0("AoN, fönster=", ma.N, ", referensintervall: ",
			     norm.min, "-",norm.max, ", medelvärde: ",
				round(mean(VitaminD$Mätvärde),1) )
subtitle.text <- paste0("Totalt ", antal, " resultat")

p.VitaminD <- ggplot(VitaminD, aes(x=datum, y=Mätvärde))+ 
 	  geom_jitter(color="grey55", alpha=1/2)+ 
  	  geom_line(aes(y=rollmean(Mätvärde, ma.N, na.pad=TRUE , col="red"))) + 
 	  coord_cartesian(ylim = c(0, 1.5*norm.max)) +
  	 annotate("text", 
			y=mean(VitaminD$Mätvärde), 
			x=as.Date(Start_Date), 
			vjust=1, 
			hjust=1,
			label=round(mean(VitaminD$Mätvärde),1))+
 		 labs(title=analys, 
 		 caption=caption.text,
 		 subtitle=subtitle.text )+
 		 xlab("Datum ")+ 
	  scale_x_date(date_breaks = "1 month", date_labels =  "%Y %b")+
 	  geom_hline(yintercept = mean(VitaminD$Mätvärde), col="blue", lty=2)+
 	  theme_minimal() +
	  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1))
 
ggsave(ExportFile, dpi=600)


# B-HbA1c (IFCC)


# Sätt intervall, filtrera ut de aktuella raderna, osv
analys <- "B-HbA1c (IFCC)"
print(analys)
ExportFile <- paste0("AoN_HbA1c", ExportFileSuffix)
norm.min.1 <-  27
norm.max.1 <- 42
norm.min.2 <-  31
norm.max.2 <- 46

HbA1c <-  filter(aondata, Analys==analys & 
		 (Mätvärde >= norm.min.1 & 
		 Mätvärde <= norm.max.1 &
		     Ålder <=50) 
		 |
		 (Mätvärde >= norm.min.2 & 
		 Mätvärde <= norm.max.2 &
		     Ålder >50) 
)

antal <- nrow(HbA1c)
ma.N <- round(antal/52, 0)
medelvärde <- round(mean(HbA1c$Mätvärde, na.rm=TRUE), 1)
antal <- nrow(HbA1c)


caption.text <- paste0("AoN, fönster=", ma.N, ", referensintervall: ",
			     norm.min.1, "-",norm.max.1, ", medelvärde: ",
				round(mean(HbA1c$Mätvärde),1) )
subtitle.text <- paste0("Totalt ", antal, " resultat")


p.HbA1c <- ggplot(HbA1c, aes(x=datum, y=Mätvärde))+ 
 	  geom_jitter(color="grey55", alpha=1/2)+ 
  	  geom_line(aes(y=rollmean(Mätvärde, ma.N, na.pad=TRUE , col="red"))) + 
 	  coord_cartesian(ylim = c(0, 1.5*norm.max.2)) +
  	 annotate("text", 
			y=mean(HbA1c$Mätvärde), 
			x=as.Date(Start_Date), 
			vjust=1, 
			hjust=1,
			label=round(mean(HbA1c$Mätvärde),1))+
 		 labs(title=analys, 
 		 caption=caption.text,
 		 subtitle=subtitle.text )+
 		 xlab("Datum ")+ 
	  scale_x_date(date_breaks = "1 month", date_labels =  "%Y %b")+
 	  geom_hline(yintercept = mean(HbA1c$Mätvärde), col="blue", lty=2)+
 	  theme_minimal() +
	  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1))
 
ggsave(ExportFile, dpi=600)




# S-GT


# Sätt intervall, filtrera ut de aktuella raderna, osv
analys <- "S-GT"
print(analys)
ExportFile <- paste0("AoN_GT", ExportFileSuffix)
norm.min.1 <-  0
norm.max.1 <- 3.5
norm.min.2 <-  0.15
norm.max.2 <- 0.75
norm.min.3 <-  0.15
norm.max.3 <- 1.3
norm.min.4 <-  0.20
norm.max.4 <- 1.9
norm.min.5 <-  0.15
norm.max.5 <- 0.75
norm.min.6 <-  0.15
norm.max.6 <- 1.2

GT <-  filter(aondata, Analys==analys & 
		 (Mätvärde >= norm.min.1 & 
		 Mätvärde <= norm.max.1 &
		     Ålder..År. <=0.5) 
		 |
		 (Mätvärde >= norm.min.2 & 
		 Mätvärde <= norm.max.2 &
		     Ålder..År. >0.5 & Ålder..År. < 18
		 ) 
		 |
		 (Mätvärde >= norm.min.3 & 
		 Mätvärde <= norm.max.3 &
		 Kön == "M" &
		     Ålder..År. >=18 & Ålder..År. < 40
		 ) 
		 |
		 (Mätvärde >= norm.min.4 & 
		 Mätvärde <= norm.max.4 &
		 Kön == "M" &
		     Ålder..År. >=40 
		 ) 
		 |
		 (Mätvärde >= norm.min.5 & 
		 Mätvärde <= norm.max.5 &
		 Kön == "K" &
		     Ålder..År. >=18 & Ålder..År. < 40
		 ) 
		 |
		 (Mätvärde >= norm.min.6 & 
		 Mätvärde <= norm.max.6 &
		 Kön == "K" &
		     Ålder..År. >40
		 ) 
)

GT <- filter(GT, datum >= Start_Date) 

antal <- nrow(GT)
ma.N <- round(antal/52, 0)
medelvärde <- round(mean(GT$Mätvärde, na.rm=TRUE), 1)
antal <- nrow(GT)


caption.text <- paste0("AoN, fönster=", ma.N, 
			     ", referensintervall: åldersjusterat",
			      ", medelvärde: ",
				round(mean(GT$Mätvärde),1) )
subtitle.text <- paste0("Totalt ", antal, " resultat")


p.GT <- ggplot(GT, aes(x=datum, y=Mätvärde))+ 
 	  geom_jitter(color="grey85", alpha=1/2)+ 
  	  geom_line(aes(y=rollmean(Mätvärde, ma.N, na.pad=TRUE , col="red"))) + 
 	  coord_cartesian(ylim = c(0, 1.5*norm.max.2)) +
  	 annotate("text", 
			y=mean(GT$Mätvärde), 
			x=as.Date(Start_Date), 
			vjust=1, 
			hjust=1,
			label=round(mean(GT$Mätvärde),1))+
 		 labs(title=analys, 
 		 caption=caption.text,
 		 subtitle=subtitle.text )+
 		 xlab("Datum ")+ 
	  scale_x_date(date_breaks = "1 month", date_labels =  "%Y %b")+
 	  geom_hline(yintercept = mean(GT$Mätvärde), col="blue", lty=2)+
 	  theme_minimal() +
	  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1))
 
ggsave(ExportFile, dpi=600)




# Ciklosporin


# Sätt intervall, filtrera ut de aktuella raderna, osv
analys <- "B-Ciklosporin"
print(analys)
ExportFile <- paste0("AoN_Ciklosporin", ExportFileSuffix)

Ciklosporin <- filter(aondata, 
			    Analys==analys 
)

Ciklosporin_alla <- Ciklosporin

Ciklosporin <- Ciklosporin[(str_detect(string = 
						  Ciklosporin$LIDN, 
					pattern = "^[1-9][0-9]{6,}") & Ciklosporin$Beställare !="MEDSERLAB") , ]

reservlid.medser <- nrow(Ciklosporin_alla) - nrow(Ciklosporin)

antal <- nrow(Ciklosporin)
#ma.N <- round(antal/52, 0)
ma.N <- round(antal/26, 0)
medelvärde <- round(mean(Ciklosporin$Mätvärde, na.rm=TRUE), 1)
antal <- nrow(Ciklosporin)


caption.text <- paste0("AoN, fönster=", ma.N, ", referensintervall: saknas",
			      ", medelvärde: ",
				round(mean(Ciklosporin$Mätvärde),1) )
subtitle.text <- paste0("Totalt ", antal, 
				" resultat (utom reserv-LID från MEDSERLAB, n=",
				reservlid.medser, ")")


p.Ciklosporin <- ggplot(Ciklosporin, aes(x=datum, y=Mätvärde))+ 
 	  geom_jitter(color="grey55", alpha=1/2)+ 
  	  geom_line(aes(y=rollmean(Mätvärde, ma.N, na.pad=TRUE , col="red"))) + 
  	 annotate("text", 
			y=mean(Ciklosporin$Mätvärde), 
			x=as.Date(Start_Date), 
			vjust=1, 
			hjust=1,
			label=round(mean(Ciklosporin$Mätvärde),1))+
 		 labs(title=analys, 
 		 caption=caption.text,
 		 subtitle=subtitle.text )+
 		 xlab("Datum ")+ 
	  scale_x_date(date_breaks = "1 month", date_labels =  "%Y %b")+
 	  geom_hline(yintercept = mean(Ciklosporin$Mätvärde), col="blue", lty=2)+
 	  theme_minimal() +
	  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1))
 
ggsave(ExportFile, dpi=600)



# Ciklosporin utan outliers (Q3+1,5*IQR)


Q3 <- quantile(Ciklosporin$Mätvärde, 0.75)
Q1 <- quantile(Ciklosporin$Mätvärde, 0.25)

IQR <- Q3-Q1
IQR.high<- 	signif(Q3+1.5*IQR,2)
IQR.low<- 	signif(Q1-1.5*IQR,2)
if(IQR.low <0){IQR.low <- 0}

Ciklosporin_no_outliers <- subset(Ciklosporin,
					    between(Ciklosporin$Mätvärde,
							Q1-1.5*IQR,
							Q3+1.5*IQR))


antal <- nrow(Ciklosporin_no_outliers)
#ma.N <- round(antal/52, 0)
ma.N <- round(antal/26, 0)
medelvärde <- round(mean(Ciklosporin_no_outliers$Mätvärde, na.rm=TRUE), 1)
antal <- nrow(Ciklosporin_no_outliers)
removed <- nrow(Ciklosporin) - antal


caption.text <- paste0("AoN, fönster=", ma.N, ", referensintervall: saknas",
			      ", medelvärde: ",
				round(mean(Ciklosporin_no_outliers$Mätvärde),1) )
subtitle.text <- paste0("Totalt ", antal, 
				" resultat, borttagna: ",
				removed, ")")

ExportFile <- paste0("AoN_Ciklosporin_NO_OUTLIERS", ExportFileSuffix)

p.Ciklosporin_no <- ggplot(Ciklosporin_no_outliers, aes(x=datum, y=Mätvärde))+ 
 	  geom_point(color="grey55", alpha=1/2)+ 
  	  geom_line(aes(y=rollmean(Mätvärde, ma.N, na.pad=TRUE , col="red"))) + 
  	 annotate("text", 
			y=mean(Ciklosporin_no_outliers$Mätvärde), 
			x=as.Date(Start_Date), 
			vjust=1, 
			hjust=1,
			label=round(mean(Ciklosporin_no_outliers$Mätvärde),1))+
 		 labs(title=paste0(analys,
					  " utan outliers (", IQR.low, "--",
					  IQR.high, ")"), 
 		 caption=caption.text,
 		 subtitle=paste0("Totalt ", nrow(Ciklosporin_no_outliers),
					" med ",
				     nrow(Ciklosporin)-nrow(Ciklosporin_no_outliers),
				     " borttagna.") )+
 		 xlab("Datum ")+ 
	  scale_x_date(date_breaks = "1 month", date_labels =  "%Y %b")+
	  scale_y_continuous(breaks=seq(0, Q3+2*(Q3-Q1), 20))+
 	  geom_hline(yintercept = mean(Ciklosporin_no_outliers$Mätvärde), col="blue", lty=2)+
 	  theme_minimal() +
	  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1))
 
ggsave(ExportFile, dpi=600)

# Takrolimus


# Sätt intervall, filtrera ut de aktuella raderna, osv
analys <- "B-Takrolimus"
print(analys)
ExportFile <- paste0("AoN_Takrolimus", ExportFileSuffix)
norm.max <- 250
norm.min <-  25

Takrolimus <- filter(aondata, 
			   Analys==analys & 
		         Datum >= Start_Date )

antal <- nrow(Takrolimus)
ma.N <- round(antal/52, 0)
medelvärde <- round(mean(Takrolimus$Mätvärde, na.rm=TRUE), 1)
antal <- nrow(Takrolimus)


caption.text <- paste0("AoN, fönster=", ma.N, ", referensintervall: saknas",
			     ", medelvärde: ",
				round(mean(Takrolimus$Mätvärde),1) )
subtitle.text <- paste0("Totalt ", antal, " resultat")


p.Takrolimus <- ggplot(Takrolimus, aes(x=datum, y=Mätvärde))+ 
 	  geom_jitter(color="grey55", alpha=1/2)+ 
  	  geom_line(aes(y=rollmean(Mätvärde, ma.N, na.pad=TRUE , col="red"))) + 
  	 annotate("text", 
			y=mean(Takrolimus$Mätvärde), 
			x=as.Date(Start_Date), 
			vjust=1, 
			hjust=1,
			label=round(mean(Takrolimus$Mätvärde),1))+
 		 labs(title=analys, 
 		 caption=caption.text,
 		 subtitle=subtitle.text )+
 		 xlab("Datum ")+ 
	  scale_x_date(date_breaks = "1 month", date_labels =  "%Y %b")+
 	  geom_hline(yintercept = mean(Takrolimus$Mätvärde), col="blue", lty=2)+
 	  theme_minimal() +
	  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1))
 
ggsave(ExportFile, dpi=600)

# Takrolimus utan outliers (Q3+1,5*IQR)


Q3 <- quantile(Takrolimus$Mätvärde, 0.75)
Q1 <- quantile(Takrolimus$Mätvärde, 0.25)

IQR <- Q3-Q1
IQR.high<- 	signif(Q3+1.5*IQR,2)
IQR.low<- 	signif(Q1-1.5*IQR,2)
if(IQR.low <0){IQR.low <- 0}

Takrolimus_no_outliers <- subset(Takrolimus, between(Takrolimus$Mätvärde,
					   Q1-1.5*IQR, Q3+1.5*IQR))


antal <- nrow(Takrolimus_no_outliers)
ma.N <- round(antal/52, 0)
medelvärde <- round(mean(Takrolimus_no_outliers$Mätvärde, na.rm=TRUE), 1)
antal <- nrow(Takrolimus_no_outliers)
removed <- nrow(Takrolimus) - antal


caption.text <- paste0("AoN, fönster=", ma.N, ", referensintervall: saknas",
			      ", medelvärde: ",
				round(mean(Takrolimus_no_outliers$Mätvärde),1) )
subtitle.text <- paste0("Totalt ", antal, 
				" resultat, borttagna: ",
				removed, ")")

ExportFile <- paste0("AoN_Takrolimus_NO_OUTLIERS", ExportFileSuffix)

p.Takrolimus_no <- ggplot(Takrolimus_no_outliers, aes(x=datum, y=Mätvärde))+ 
 	  geom_point(color="grey55", alpha=1/2)+ 
  	  geom_line(aes(y=rollmean(Mätvärde, ma.N, na.pad=TRUE , col="red"))) + 
  	 annotate("text", 
			y=mean(Takrolimus_no_outliers$Mätvärde), 
			x=as.Date(Start_Date), 
			vjust=1, 
			hjust=1,
			label=round(mean(Takrolimus_no_outliers$Mätvärde),1))+
 		 labs(title=paste0(analys,
					  " utan outliers (", IQR.low, "--",
					  IQR.high,")"), 
 		 caption=caption.text,
 		 subtitle=paste0("Totalt ", nrow(Takrolimus_no_outliers),
					" med ",
				     nrow(Takrolimus)-nrow(Takrolimus_no_outliers),
				     " borttagna.") )+
 		 xlab("Datum ")+ 
	  scale_x_date(date_breaks = "1 month", date_labels =  "%Y %b")+
	  scale_y_continuous(breaks=seq(0, Q3+2*(Q3-Q1), 1))+
 	  geom_hline(yintercept = mean(Takrolimus_no_outliers$Mätvärde), col="blue", lty=2)+
 	  theme_minimal() +
	  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1))
 
ggsave(ExportFile, dpi=600)


