library(tidyverse)

DateToday = Sys.Date()

# Titles and such
PlotTitle <- paste0("Project title")
XAxisTitle <- "Task"
YAxisTitle <- "Date"
ExportFile <- "ProjectGantt.png"
SourceFile <- "gantt-data.csv"

# Source table format (csv-file)
# Sort;TaskName;Person;Status;DateType;Date


df_gantt <- read.csv2(SourceFile,)
df_gantt$Status <- as.factor(df_gantt$Status)
df_gantt$Date <- as.Date(df_gantt$Date)
df_gantt <- df_gantt[order(df_gantt$Sort), ]   # Sort by sorting no

GanttTable <- ggplot() +
    geom_line(data=df_gantt,
		  mapping=aes(x=fct_rev(fct_inorder(TaskName)), y=Date, color=Person, alpha=Status), size=10) +
    geom_hline(yintercept=as.Date(DateToday), colour="black", linetype="dashed") +
    coord_flip() +
    scale_alpha_discrete(range=c(1, 0.4), guide="none") +
    scale_y_date(date_breaks = "1 month") +
    labs(title=PlotTitle,
	   caption=DateToday, 
         x = XAxisTitle,
         y = YAxisTitle,
         colour = "Person") +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 90),
          panel.grid.minor = element_line(colour="white", size=0.5),
          legend.position="right",
          plot.title = element_text(hjust = 0.5))

GanttTable
ggsave(ExportFile, dpi=600)
