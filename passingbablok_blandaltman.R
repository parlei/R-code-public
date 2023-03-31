#' Skapar Passing-Bablok och Bland Altman för ett antal analyser
#' Pär Leijonhufud 2023-03-30
#'
#' TODO
#'   * Snygga upp BA-grafen lite? etikettera +/- 2SD, osv
#'   * Utveckla funktionen så att den kan hamna i ett bibliotek och vara
#'     mer generell
#'
#' @export


#' @import tidyverse
#' @import mcr
#' @import ggplot2
#' @import ggExtra
#' @import gridGraphics
#' @import cowplot

# Funktion för att skapa plotterna
pb.ba <- function(ANALYS, TYP, x_label, y_label, SUBTITLE_TEXT, TITLE_TEXT, PLOT_FILENAME, XDATA, YDATA){

	  # Hoppa om alla är NA
	  if(dateutils::allNA(XDATA)){return(0)}
	  if(dateutils::allNA(YDATA)){return(0)}

	  # Vilken analys och typ
	  message(paste(ANALYS, TYP))

	  # Skapa lite textblobbar
	  # Passing Bablok
	  title.text <- TITLE_TEXT
	  sub.text <- SUBTITLE_TEXT
	  x.text <- x_label
	  y.text <- y_label
	  #PB.PLOT_FILENAME <- paste0(TYP, "_",ANALYS, "_PB.png")
	  #BA.PLOT_FILENAME <- paste0(TYP, "_",ANALYS, "_BA.png")
	  PB_BA.filename <- PLOT_FILENAME
	 
	  # Bland Altman labels
	  x.text.BA <- "Medelvärde"
	  y.text.BA <- paste0("Avvikelse (", y_label, " - ", x_label, ")")
	  title.text.BA <- paste("Bland-Altman ", ANALYS)
	  sub.text.BA <- title.text
	  caption.BA <- sub.text

	  # Plot: Passing-Bablok
	  PB.reg <- mcreg(
				XDATA, YDATA,
		    method.reg = "PaBa",
		    na.rm=TRUE,
		    mref.name=x.text,       # optional label for X-axis data
		    mtest.name=y.text       # optional label for Y-axis data
        )


        # produce the graph
	  # Sparas inte som bild då båda braferna slås ihop till en graf

	  #png(PB.filename)
        MCResult.plot(PB.reg, equal.axis = TRUE,
            points.col = "#FF7F5060",
            points.pch = 19,
            ci.area = TRUE,
            ci.area.col = "#0000FF50",
            main = title.text,
            sub = sub.text,
            add.grid = FALSE,
            points.cex = 1
            )
	  # dev.off()

	  # spara som objekt
	  plot_PB <- recordPlot()


	  # Bland Altman
	  
	  # Skapa lite blobbar
	  ba.antal <- length(XDATA)

	  medelvärde <- (YDATA+XDATA)/2
	  avvikelse <- (YDATA-XDATA)

	  ba.2sdplus <- mean(avvikelse, na.rm=TRUE)+ 2*sd(avvikelse,
									  na.rm=TRUE)
	  ba.2sdminus <- mean(avvikelse, na.rm=TRUE)- 2*sd(avvikelse,
									   na.rm=TRUE)
	  ba.avvikelse.medel <- mean(avvikelse, na.rm=TRUE)
	  
	  ba.2sdplus.text <- paste0("+2SD=", signif(ba.2sdplus, 3))
	  ba.2sdminus.text <- paste0("+2SD=", signif(ba.2sdminus, 3))
	  ba.avvikelse.medel.text <- paste0("medel=",
							signif(ba.avvikelse.medel, 3))

	  
	  # Sätt max och min för grafen
	  ymin <- mean(avvikelse, na.rm=TRUE)- 3*sd(avvikelse, na.rm=TRUE)
	  ymax <- mean(avvikelse, na.rm=TRUE)+ 3*sd(avvikelse, na.rm=TRUE)
	  
	  if(ymin > min(avvikelse)){
			 ymin <- min(avvikelse)
		    }

	  if(ymax < max(avvikelse)){
			 ymax <- max(avvikelse)
		    }

	  # Lägg data i en temporär df som ggplot kan använda
	  df <- data.frame(medelvärde = medelvärde, avvikelse = avvikelse)

	  # ggplot2
	  p.ba <- ggplot(df, aes(x=medelvärde, y=avvikelse))+
		    geom_point(alpha=0.5)+
		    geom_hline(yintercept = 0, colour="black", size=0.5, linetype=3)+
		    geom_hline(yintercept = ba.avvikelse.medel, colour="blue", size=0.5)+
		    geom_hline(yintercept = ba.2sdplus, colour="red", size=0.5, linetype=2)+
		    geom_hline(yintercept = ba.2sdminus, colour="red", size=0.5, linetype=2)+
        
		    geom_smooth(method=lm, linetype=4) +  # Add linear regression line
        
		    annotate("text", x=-Inf, y=mean.ba.avvikelse, label=ba.avvikelse.medel.text, hjust=1)+
		    annotate("text", x=-Inf, y=ba.2sdplus, label=ba.2sdplus.text, hjust=1)+
		    annotate("text", x=-Inf, y=ba.2sdminus, label=ba.2sdminus.text, hjust=1)+
        
		    labs(
			   title=title.text.ba,
			   caption = caption.BA
			   )+
		    ylab(y.text.BA)+
		    xlab(x.text.BA)+
		    theme_bw()+
		    coord_cartesian(ylim=c(ymin, ymax))

	  #ggsave(BA.filename, dpi=50)


        
        # Grid with bothplots
        plot_grid(plot_PB, p.ba, ncol=2)
        
        # Save as a file. Please note that the width might need to be adjusted for the plots to come out ok.
        # Try setting the width to double that of the ggplot object
	  if(!is.na(PLOT_FILENAME)){
		    ggsave(PB_BA.filename, units = "in", width = 21)
	  }

}
