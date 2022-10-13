library(tidyverse)
library(lubridate)
library(ggpubr)
library(rstatix)
library(coin)

# Ange "suffix" på aktuell fil
period <- "långtid"


########### statiskt  ###########
# infil <- paste0("patientmedian_", period, ".csv")
 exportfil <- paste0("patientmedian_resultat_", period, "_MÅNAD.csv")
 exportfil.Q <- paste0("patientmedian_resultat_", period, "_KVARTAL.csv")

#  rådata.2020.q1 <- read.csv2("patientmedian_2020_q1.csv", fileEncoding = "UTF-8-BOM")
#  rådata.2020.q2 <- read.csv2("patientmedian_2020_q2.csv", fileEncoding = "UTF-8-BOM")
#  rådata.2020.q3 <- read.csv2("patientmedian_2020_q3.csv", fileEncoding = "UTF-8-BOM")
#  rådata.2020.q4 <- read.csv2("patientmedian_2020_q4.csv", fileEncoding = "UTF-8-BOM")
#  rådata.2021.q1 <- read.csv2("patientmedian_2021_q1.csv", fileEncoding = "UTF-8-BOM")
#  rådata.2021.q2 <- read.csv2("patientmedian_2021_q2.csv", fileEncoding = "UTF-8-BOM")
#  rådata.2021.q3 <- read.csv2("patientmedian_2021_q3.csv", fileEncoding = "UTF-8-BOM")
#  rådata.2021.q4 <- read.csv2("patientmedian_2021_q4.csv", fileEncoding = "UTF-8-BOM")
#  rådata.2022.q1 <- read.csv2("patientmedian_2022_q1.csv", fileEncoding = "UTF-8-BOM")
#  rådata.2022.q2 <- read.csv2("patientmedian_2022_q2.csv", fileEncoding = "UTF-8-BOM")
#  rådata.2022.q3 <- read.csv2("patientmedian_2022_q3.csv", fileEncoding = "UTF-8-BOM")
#  
#   rådata <- rbind(rådata.2020.q1, rådata.2020.q2, rådata.2020.q3,
#  			rådata.2020.q4, rådata.2021.q1, rådata.2021.q2,
#  			rådata.2021.q3, rådata.2021.q4, rådata.2022.q1,
#  			rådata.2022.q2, rådata.2022.q3)

rådata <- read_csv2(list.files(path = ".", pattern="patientmedian_20[0-9]+_q[1-4].csv"))

# Rensa
rådata <- filter(rådata, 
			Analys != "" | !is.na(Mätvärde) | 
				  Analys !="S-Klorid" |
				  Analys != "P-Laktat" |
				  Analys != "eGFR Medel, Relativt"|
				  Analys != "Pt - eGFR(Kreatinin)"
)

# Datum
rådata$Datum <- as.Date(rådata$"Tekniskt godkännande", na.rm=TRUE)
rådata$year.month <- strftime(rådata$Datum, format="%Y.%m")
rådata$year.month <- as.factor(rådata$year.month)
rådata$kvartal <- as.factor(quarter(rådata$Datum, with_year = TRUE))
rådata$kvartal2 <- as.character(rådata$kvartal)

# skapa tomma DF för att sammanställa 
patmed.all <- data.frame("year.month"=NA, "analys"=NA, "antal"=NA,
				 "median"=NA,   "mean"=NA, "sd"=NA)
patmed.all.Q <- data.frame("kvartal"=NA, "analys"=NA, "antal"=NA,
				 "median"=NA,  "mean"=NA, "sd"=NA)

# Sammansatta analysvis, månad och kvartal
for(analys in unique(rådata$Analys)){print(analys) 

	data <- filter(rådata, Analys == analys)
	
	# Ställ samman
  	# år-månad 	
	patmed <- data %>%
	 	  group_by(year.month) %>%
	 	  summarise(
				 analys = max(Analys ) , 
				 antal = sum(Antal, na.rm=TRUE), 
				 median = median(Mätvärde, na.rm=TRUE),
				 mean = mean(Mätvärde, na.rm=TRUE), 
				 sd = sd(Mätvärde, na.rm=TRUE)
		  )
	
	# lägg ihop
	patmed.all <- rbind(patmed.all, patmed)

	# Kvartal
	patmed <- data %>%
	 	  group_by(kvartal) %>%
	 	  summarise(
				 analys = max(Analys ), 
				 sd = sd(Mätvärde, na.rm=TRUE), 
				 antal = sum(Antal, na.rm=TRUE), 
				 median = median(Mätvärde, na.rm=TRUE), 
				 mean = mean(Mätvärde, na.rm=TRUE)
			)
	
	patmed.all.Q <- rbind(patmed.all.Q, patmed)

	# Städa
	patmed.all.Q <- filter(patmed.all.Q, !is.na(analys))
	patmed.all.Q <- filter(patmed.all.Q, !is.na(kvartal))
	

}

# skapa grafer, två per analys, som sedan slås ihop
 for(a in unique(patmed.all.Q$analys)){
	  print(a)	# debug + progress report

	  n.analys <- nrow(filter(rådata, Analys == a))

	  # Filnamn
 	  filename.k <- paste0(a,"_kvartal.png")
 	  filename.m <- paste0(a,"_månad.png")
 	  filename.b <- paste0(a,"_månad+kvartal.png")

	  # sista och näst sista kvartalen: det är dessa vi räknar på
	  # skillanden mellan
	  # samt räkna ut effect size
	  kvartal.sist <- sort(unique(rådata$kvartal2))[length(unique(rådata$kvartal2))]

	  kvartal.nästsist <- sort(unique(rådata$kvartal2))[length(unique(rådata$kvartal2))-1]


	  # utdrag
	  q1.q2<- ((filter(rådata,Analys==a, kvartal==kvartal.nästsist| kvartal==kvartal.sist)))


	  # Effect size
	  ES.es <- q1.q2 %>% 
		    wilcox_effsize(Mätvärde ~ kvartal2) %>% 
		    getElement("effsize")

	  ES.magn <- q1.q2 %>% 
		    wilcox_effsize(Mätvärde ~ kvartal2) %>% 
		    getElement("magnitude")

	  # medianen för analysen
 	  patmed.analys <- round(median(filter(patmed.all,
 							   analys==a)$median),3)

 	  subtitle.a <- paste0(
					"Median över perioden: ", 
					patmed.analys 
				)

 	  subtitle.ak <- paste0(
					 "Median över perioden: ", 
					 patmed.analys, 
					 "  Effect size (sista två kvartalen): ", 
					 signif(ES.es, 2), " (", ES.magn, ")"
				)


	  caption.k <- paste0("Effect size baseras på Wilcoxon r, 
				    mellan de två senaste kvartalen.
				    Totalt: ", n.analys)
 
	  # plot max+min
 	  maxlevel <- 1.1* max(filter(patmed.all.Q, analys==a)$median)
 	  minlevel <- 0.9* min(filter(patmed.all.Q, analys==a)$median)

	  plot.kvartal<-  ggplot((filter(patmed.all.Q, analys==a)), 
 		   aes(x=factor(kvartal), y=median, group=1)) + 
 		    geom_line() + 
 		    geom_point()+
		    geom_text(aes(label=median), vjust = -0.5)+
 		    geom_hline(yintercept=patmed.analys, col="blue")+ # medianlinje
 		    coord_cartesian(y=c(minlevel,maxlevel)) +  
  		    scale_x_discrete(breaks=c( 	# uppdateras ibland
  							"2018.1", "2018.2", "2018.3", "2018.4", 
  							"2019.1", "2019.2", "2019.3", "2019.4", 
  							"2020.1", "2020.2", "2020.3", "2020.4", 
  						     "2021.1", "2021.2", "2021.3", "2021.4",
  						     "2022.1", "2022.2", "2022.3", "2022.4",
  						     "2023.1", "2023.2", "2023.3", "2023.4"
  					    ))+
 		    labs(title=a, 
 			   subtitle=subtitle.ak,
			   caption=caption.k)+
 		    ylab("Patientmedian")+
 		    xlab("Kvartal")+
 		    theme_minimal()
 

 	  maxlevel <- 1.1* max(filter(patmed.all, analys==a)$median)
 	  minlevel <- 0.9* min(filter(patmed.all, analys==a)$median)
 
 	  plot.månad <- ggplot((filter(patmed.all, analys==a)), 
 		   aes(x=factor(year.month), y=median, group=1)) + 
 		    geom_line() + 
 		    geom_point()+
 		    geom_hline(yintercept=patmed.analys, col="blue")+
 		    coord_cartesian(y=c(minlevel,maxlevel)) +
  		    scale_x_discrete(breaks=c( 
  							"2018.01", "2018.04", "2018.07", "2018.10", 
  							"2019.01", "2019.04", "2019.07", "2019.10", 
  							"2020.01", "2020.04", "2020.07", "2020.10", 
  						     "2021.01", "2021.04", "2021.07", "2021.10",
  						     "2022.01", "2022.04", "2022.07", "2022.10",
  						     "2023.01", "2023.04", "2023.07", "2023.10"
  					    ))+
 		    labs(title=a, 
 			   subtitle=subtitle.a)+
 		    theme_minimal()+
 		    ylab("Patientmedian")+
 		    xlab("Månad")
 
# ställ samman i gemensam graf  och spara
plot.båda <- ggarrange(plot.månad, plot.kvartal, nrow=2, labels="auto")

plot.båda
ggsave(filename.b)

 }

# Spara en exportfil
write.csv2(patmed.all, file=exportfil)
write.csv2(patmed.all.Q, file=exportfil.Q)
