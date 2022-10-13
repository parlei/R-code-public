# Sammanställa våra verkliga svarstider (TAT, Turn Around Time) på
# samtliga analyser.
# 
# Drugt 6 miljoner rader initialt, körtid mindre än 1 minut (mest
# importen)
#
# Pär Leijonhufvud/2022-10-05
#
# Lade till sortering på CobasPro-modul PL/2022-10-10
#
# lade till Pos/Neg på virusaanalyserna, samt grupperade beställande
# enthet på virus. PL/2022-10-13


library(tidyverse)
library(xlsx)
library(knitr)
library(rmarkdown)


## Importera data: 1 fil per månad (202101-allt.csv, osv)
files <- list.files(path = ".", pattern = "20[0-9]+-allt.csv" )

allt.21.22 <- read_csv2(files)

# Lista över analyser på C- resp E-modul på Cobas, samt virus-analys
analyser.kodade <- read.csv2("analyser-kodade.csv", fileEncoding = "UTF-8-BOM")


## Data cleaning

# ledtid till period
allt.21.22$Ledtid2 <-  lubridate::hms(allt.21.22$Ledtid)

# Ankomsttid som datum
allt.21.22$Ankomsttid <-   lubridate::ymd_hm(allt.21.22$Ankomsttid)

# ledtid till sekunder och minuter (min är lämplig enhet)
allt.21.22$Ledtid.s <- lubridate::period_to_seconds(allt.21.22$Ledtid2)
allt.21.22$Ledtid.m <- round(lubridate::period_to_seconds(allt.21.22$Ledtid2)/60, 0)

# Städa bort skräp: beställa eller ankomna mer än en månad före perioden
allt.21.22 <- filter(allt.21.22, 
			   Registreringstid > "2020-12-01" &
			   Ankomsttid > "2020-12-01" &
			   !is.na(Mätvärde) & 
			   !is.na(Ledtid) & 
			   !is.na(Ledtid.m) &
			   Ledtid != "-"
)

# Bara Cobas, med Typ == modul på Cobas, eller "E-Virus" för
# virusanalyser: i praktiken en "left join"
allt.21.22.cobas <- merge(allt.21.22, analyser.kodade, by = "Analys")

# Sortera ut virus
allt.virus <- filter(allt.21.22.cobas, Typ == "E-Virus")

# Tomma kolumner för Pos/Neg och kundgrupp (ÖSJ, BlodC, "resten")
allt.virus[ , "Pos.Neg"] <- NA
allt.virus[ , "kundgrupp"] <- NA

# Lägg till Pos/Neg i Pos.Neg, samt ÖSJ/BlodC/<resten> i kundgrupp
allt.virus <- allt.virus %>% 
	  mutate(Pos.Neg = replace(Pos.Neg, Analys == "S-anti-HAV" & Mätvärde < 1, "Neg"),
	  Pos.Neg = replace(Pos.Neg, Analys == "S-anti-HAV IgM" & Mätvärde < 1, "Neg"),
	  Pos.Neg = replace(Pos.Neg, Analys == "S-anti-HAV IgM" & Mätvärde < 1, "Neg"),
	  Pos.Neg = replace(Pos.Neg, Analys == "S-anti-SARS-CoV-2" & Mätvärde < 20, "Neg"),
	  Pos.Neg = replace(Pos.Neg, Analys == "S-antiHBc" & Mätvärde >= 1, "Neg"),
	  Pos.Neg = replace(Pos.Neg, Analys == "S-antiHTLV I/II" & Mätvärde < 1, "Neg"),
	  Pos.Neg = replace(Pos.Neg, Analys == "S-antiSyfilis" & Mätvärde < 1, "Neg"),
	  Pos.Neg = replace(Pos.Neg, Analys == "S-HBsAg" & Mätvärde < 1, "Neg"),
	  Pos.Neg = replace(Pos.Neg, Analys == "S-HCV Ag" & Mätvärde < 1, "Neg"),
	  Pos.Neg = replace(Pos.Neg, Analys == "S-HIV 1+2 (ag+ak)" & Mätvärde < 0.9, "Neg"),
	  Pos.Neg = replace(Pos.Neg, Analys == "S-anti-Rubella IgG (kvant)" , "Num"),
	  Pos.Neg = replace(Pos.Neg, is.na(Pos.Neg), "Pos"),
	  kundgrupp = replace(kundgrupp, str_detect(Beställare, "^ÖSJ"), "ÖSJ"),
	  kundgrupp = replace(kundgrupp, Beställare == "ÖSJ   MEDSERBLC", "BlodC")

)

## Summera
# Med avrundade värden (heltal). Tre olika

# Samtliga, per analys
TAT.summary <- allt.21.22.cobas %>% 
	  group_by(Analys) %>%
	  summarise(
			Antal = length(Ledtid.s), 
			Medel = round(mean(Ledtid.m, na.rm=TRUE), 0), 
			Median = round(median(Ledtid.m, na.rm=TRUE), 0), 
			Min = min(Ledtid.m, na.rm=TRUE), 
			Max = max(Ledtid.m, na.rm=TRUE), 
			Q75 = round(quantile(Ledtid.m, 0.75, na.rm=TRUE), 0),
			Q90 = round(quantile(Ledtid.m, 0.90, na.rm=TRUE), 0),
			Q95 = round(quantile(Ledtid.m, 0.95, na.rm=TRUE), 0),
			Q99 = round(quantile(Ledtid.m, 0.99, na.rm=TRUE), 0)
	  )

# Samtliga Cobas, på C/E/Virus
TAT.summary.cobas <- allt.21.22.cobas %>% 
	  group_by(Typ) %>%
	  summarise(
			Antal = length(Ledtid.s), 
			Medel = round(mean(Ledtid.m, na.rm=TRUE), 0), 
			Median = round(median(Ledtid.m, na.rm=TRUE), 0), 
			Min = min(Ledtid.m, na.rm=TRUE), 
			Max = max(Ledtid.m, na.rm=TRUE), 
			Q75 = round(quantile(Ledtid.m, 0.75, na.rm=TRUE), 0),
			Q90 = round(quantile(Ledtid.m, 0.90, na.rm=TRUE), 0),
			Q95 = round(quantile(Ledtid.m, 0.95, na.rm=TRUE), 0),
			Q99 = round(quantile(Ledtid.m, 0.99, na.rm=TRUE), 0)
	  )

# virus, uppdelade på flera undergrupper p.g.a. att positiva skall
# skickas på verifiering (Umeå, tar tid)
TAT.summary.virus <- allt.virus %>% 
	  group_by(kundgrupp, Pos.Neg, Analys) %>%
	  summarise(
			Antal = length(Ledtid.s), 
			Medel = round(mean(Ledtid.m, na.rm=TRUE), 0), 
			Median = round(median(Ledtid.m, na.rm=TRUE), 0), 
			Min = min(Ledtid.m, na.rm=TRUE), 
			Max = max(Ledtid.m, na.rm=TRUE), 
			Q75 = round(quantile(Ledtid.m, 0.75, na.rm=TRUE), 0),
			Q90 = round(quantile(Ledtid.m, 0.90, na.rm=TRUE), 0),
			Q95 = round(quantile(Ledtid.m, 0.95, na.rm=TRUE), 0),
			Q99 = round(quantile(Ledtid.m, 0.99, na.rm=TRUE), 0)
	  )


# Exportera: NB: kommer att skriva över filen!
write.xlsx2(TAT.summary.cobas, file="Tat-tider_21-22_minuter_typ2.xlsx",
		sheetName="TAT-tider 2021-22 Cobas")

write.xlsx2(as.data.frame(TAT.summary.virus), file="Tat-tider_21-22_minuter_typ2.xlsx",
		sheetName="TAT-tider 2021-22 virus", append=TRUE)

write.xlsx2(as.data.frame(TAT.summary), file="Tat-tider_21-22_minuter_typ2.xlsx",
		sheetName="TAT-tider 2021-22 analyser", append=TRUE)

write.xlsx2(analyser.kodade, file="Tat-tider_21-22_minuter_typ2.xlsx",
		sheetName="CobasPro-analyser moduler", append=TRUE)

