# Rullande median och veckomedelvärde för några analyser
# Baserat på AON-graferna
#
# Pär Leijonhufvud/2023-03-17
#
# TODO
# * Veckomedel justeras till månadsmedel om veckan har för få
# * Veckomedel exkluderar outliers
# * Medianen exkluderar outliers
#
# Om fönstret (1 vecka) blir mindre än 10 adderas veckor tills fönstret
# överstiger 10 2022-11-21/PL
#
# Skapar både en veckomedelvärde och en rullande median PL/2023-03-17

library(zoo)
library(tidyverse)
library(ggplot2)
library(lubridate)
library(gridExtra)
library(ISOweek)

today <- Sys.Date()

# Importera alla filer
list_files <- list.files(path=".", pattern = "patientmedian[a-zA-Z0-9_]*.csv")

rådata <- read_csv2(list_files)

total_rows <- nrow(rådata)

# Rensa: utan analys, eller duplicerade
rådata <- filter(rådata, !is.na(Analys))
rådata <- filter(rådata, Analys != "")
rådata <- rådata %>% 
	  distinct(.keep_all = TRUE)

total_rows_distinct <- nrow(rådata)

message(paste0("Totalt: ", total_rows_distinct, " rensade ", total_rows -
	  total_rows_distinct , " duplicerade rader"))

# Bara ett år tillbaka i tiden
Start_Date <- as.Date(Sys.Date() -365)

# Spara bilder med suffix som inkluderar datatum
ExportFileSuffix <- paste0("-regelbunden_",Sys.Date(),".png")
ExportFileSuffix <- paste0("-övervakning.png")

# Exportfil från QlikView, sparas med samma filnamn varje gång
#ROLL_MED_DATA <- read.csv2("aon_vanliga.csv", fileEncoding = "UTF-8-BOM")

ROLL_MED_DATA <- rådata


# Ta bort rader utan LID och färdigt resultat
ROLL_MED_DATA <- subset(ROLL_MED_DATA, !is.na(LIDN))
ROLL_MED_DATA <- subset(ROLL_MED_DATA, "Tekniskt godkännande" !="")


# fixa datum, vecka, år: allt används inte just nu, men...
ROLL_MED_DATA$datum <- as.Date(ROLL_MED_DATA$Registreringstid, "%Y-%m-%d")
ROLL_MED_DATA$year <- format(as.Date(ROLL_MED_DATA$datum, format="%Y-%m-%d"),"%Y")
ROLL_MED_DATA$week <- strftime(ROLL_MED_DATA$datum, format="%V")
ROLL_MED_DATA$ISOweek <- ISOweek(ROLL_MED_DATA$datum)
ROLL_MED_DATA$Datum <- as.POSIXct(ROLL_MED_DATA$"Tekniskt godkännande", na.rm=TRUE)

# Utan datum tas bort
ROLL_MED_DATA <- filter(ROLL_MED_DATA, !is.na(datum))

# Bara senaste året
ROLL_MED_DATA <- filter(ROLL_MED_DATA, datum > Start_Date)

message(paste0("Senaste året: ", nrow(ROLL_MED_DATA)))

# Function för att räkna upp ma.N
winsize <- function(window){
	  ma.N.initial <- ma.N
	  weekcount <- 1
	  while(ma.N < 10){ 
		    
		    weekcount <- weekcount+1 
		    ma.N <- ma.N + ma.N.initial 
	  } 
	  c(ma.N, weekcount)
 }

# Funktion för att skapa grafer
rollmed_plot <- function(ANALYSNAMN){
	  


	  # Sätt exortfilnamn
	  ExportFile <- paste0("RollMedian_", ANALYSNAMN, ExportFileSuffix)
	  ExportFile.line <- paste0("Veckomedel_", ANALYSNAMN, ExportFileSuffix)
	  
	  # Filtrera fram aktuell analys
	  ROLL_MED_DATA.tmp <- filter(
						ROLL_MED_DATA, Analys==ANALYSNAMN 
	  )

	  # Fönsterstorlek: en eller flera veckor tills minst 20
	  antal <- nrow(ROLL_MED_DATA.tmp)
	  ma.N <- round(7 * antal/365, 0)

	  ma.N.initial <- ma.N 


	  # Skriv ut analysnamnet och antalet/vecka
	  message(ANALYSNAMN," ", ma.N.initial, "/vecka \n")
	  

	  while(ma.N < 20){ 
		    ma.N <- ma.N + ma.N.initial
	  }


	  # För att kunna exkludera outliers
	  Q3 <- quantile(ROLL_MED_DATA.tmp$Mätvärde, 0.75)
	  Q1 <- quantile(ROLL_MED_DATA.tmp$Mätvärde, 0.25)
	  
	  IQR <- Q3-Q1
	  IQR.high<- 	signif(Q3+1.5*IQR,2)
	  IQR.low<- 	signif(Q1-1.5*IQR,2)
	  if(IQR.low <0){
		    IQR.low <- 0
	  }

	  data_outliers <- filter(ROLL_MED_DATA.tmp, (Mätvärde > IQR.high) | (Mätvärde <
		   IQR.low))

	  # För att skriva ut medel och median (totala)
	  if(median(ROLL_MED_DATA.tmp$Mätvärde, na.rm=TRUE) < 1 ){
	  medelvärde <- round(mean(ROLL_MED_DATA.tmp$Mätvärde, na.rm=TRUE), 3)
	  medianvärde <- round(median(ROLL_MED_DATA.tmp$Mätvärde, na.rm=TRUE), 3)
	  } else {
	  medelvärde <- round(mean(ROLL_MED_DATA.tmp$Mätvärde,
					    na.rm=TRUE), 1)
	  medianvärde <- round(median(ROLL_MED_DATA.tmp$Mätvärde,
						 na.rm=TRUE), 1)
	  }

	  # För att placera linjer: exakta värden
	  medelvärde.all <- mean(ROLL_MED_DATA.tmp$Mätvärde, na.rm=TRUE)
	  medianvärde.all <- median(ROLL_MED_DATA.tmp$Mätvärde, na.rm=TRUE)
	  
	  # Används inte idag
	  plus196sd <- medelvärde + 1.96*sd(ROLL_MED_DATA.tmp$Mätvärde, na.rm=TRUE)
	  minus196sd <- medelvärde - 1.96*sd(ROLL_MED_DATA.tmp$Mätvärde, na.rm=TRUE)

	  # Sätt min och max för grafen
	  ymax <-  2.0 * IQR.high
	  ymin <-  0.5 * IQR.low

	  if(ymin < 0){ymin <- 0}
	 
	  # Caption och subtitle med information
	  
	  title.text <- paste(ANALYSNAMN, "(rullande median)")

	  caption.text <- paste0("Rullande median, fönster=", ma.N, 
					 ", medelvärde: ", medelvärde, 
					 "  median: ", medianvärde)
	  caption.medel.text <- paste0("Veckomedelvärde: totalt medelvärde: ",
						 medelvärde, " (medel +/-SEM)")
	  subtitle.text <- paste0("Totalt ", antal, " resultat, ", Start_Date,
	  				"--", as.Date(Sys.Date()),
					", ", ma.N.initial, " per vecka")

	  
	  # Skapa graf
	  plot.rullmedian <- ggplot(ROLL_MED_DATA.tmp, aes(x=datum, y=Mätvärde))+ 
	    	  #geom_line(aes(y=rollmedian(Mätvärde, ma.N, na.pad=TRUE )), color="red") + 
	    	  #geom_line(aes(y=rollmean(Mätvärde, ma.N, na.pad=TRUE )), color="blue") + 
		    geom_point(data=data_outliers, aes(x=datum, y=Mätvärde),  
				    color="red", alpha=1/5)+
	   	  coord_cartesian(ylim = c(ymin, ymax))+
	    	 annotate("text", y=medelvärde.all, 
	  			x=as.Date(Start_Date), 
	  			vjust=-1, 
				hjust=0,
	  			label=paste0("medel=", medelvärde))+
	    	 annotate("text", y=medianvärde.all, 
	  			x=as.Date(Start_Date), 
	  			vjust=1,
				hjust=0,
	  			label=paste0("median=", medianvärde))+
	   		 labs(title=title.text, 
	   		 caption=caption.text,
	   		 subtitle=subtitle.text )+
	   		 xlab("Datum ")+ 
	  	  scale_x_date(date_breaks = "1 month", date_labels =  "%Y %b")+
	   	  geom_hline(yintercept = medelvärde.all, col="blue", lty=2)+
	   	  geom_hline(yintercept = medianvärde.all, col="red", lty=2)+
	   	  theme_bw() +
	  	  theme(axis.text.x = element_text(angle = 45, vjust = 0.5,
								 hjust=1))

		  # Visar punkter om rimligt (ca 30/vecka)
  		  if(ma.N.initial < 31){
  		    plot.rullmedian <- plot.rullmedian +
				geom_point(color="grey85", alpha=1/2)+
				geom_point(data=data_outliers, 
					     aes(x=datum, y=Mätvärde),  
					     color="red", alpha=1/5)+
				geom_line(aes(y=rollmedian(Mätvärde, ma.N, na.pad=TRUE )), color="red") 
		  } else {
  		    plot.rullmedian <- plot.rullmedian +
				geom_line(aes(y=rollmedian(Mätvärde, ma.N,
								   na.pad=TRUE )),
					    color="red") #+ geom_smooth(aes(y=rollmedian(Mätvärde, ma.N, na.pad=TRUE )), color="red") 
				           geom_point(data=data_outliers, 
							    aes(x=datum, y=Mätvärde),  
							    color="red", alpha=1/5)
		  }

	   
	  ggsave(ExportFile, dpi=600)

	  ### Veckomedelvärde
	  # Utkommenterat då den inte ger så mycket

	  # Sammanfattande tabell
## 	  df <- ROLL_MED_DATA.tmp %>%
## 	  	   group_by(ISOweek) %>%
## 	  	   summarise(
## 				   Median = median(Mätvärde), 
## 				   Medel = mean(Mätvärde), 
## 				   N = length(Mätvärde),
## 				   sd = sd(Mätvärde), 
## 				   SEM = sd(Mätvärde)/sqrt(length(Mätvärde)) 
## 	  	   )
## 	  # För att få en läsbar x-axel
## 	  xlabels <- sort(unique(ROLL_MED_DATA.tmp$ISOweek))
## 	  xlabels[seq(2, length(xlabels), 2)] <- ""
## 
## 	  # Plotta linje med +/- SEM
## 	  lineplot <- ggplot(df, aes(x=ISOweek , y=Medel, group=1))+
## 	  	  geom_point()+
## 	  	  geom_line()+
## 	  	  geom_errorbar(aes(ymin=Medel-SEM, ymax=Medel+SEM), width=0.2)+
## 	  	  labs(title=ANALYSNAMN,
## 	   		 caption=caption.medel.text,
## 	   		 subtitle=subtitle.text )+
## 			   xlab("Vecka")+
## 	  	  geom_hline(yintercept = medelvärde.all, col="blue", lty=2)+
## 	  	  coord_cartesian(ylim = c(ymin, ymax))+
## 	  	   	  theme_bw() +
## 	  	  	  theme(axis.text.x = element_text(angle = 45, vjust =
## 									   0.5, hjust=1))+
## 		  scale_x_discrete(labels=xlabels)
## 	  
## 	  	  ggsave(ExportFile.line, dpi=600)

}

# Kör funktionen för varje analyt
for(A in unique(ROLL_MED_DATA$Analys)){
	  rollmed_plot(A)


}



