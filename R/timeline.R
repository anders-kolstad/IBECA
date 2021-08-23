
# Start -------------------------------------------------------------------



library(ggplot2)
library(scales)
library(lubridate)
library(readxl)




# Data --------------------------------------------------------------------

df <- read_excel("data/milestones.xlsx")

#The first thing we’ll do is define a date for each of these rows as the 1st of the month.

df$date <- with(df, ymd(sprintf('%04d%02d%02d', year, month, 1)))
df <- df[with(df, order(date)), ]
head(df)



#Next we’ll convert the status to an ordinal categorical variable, in order of criticality ranging from “Complete” to “Critical”. We’ll also define some hexadecimal colour values to associate with these statuses.

#status_levels <- c("Complete", "On Target", "At Risk", "Critical")
#status_colors <- c("#0070C0", "#00B050", "#FFC000", "#C00000")
#df$status <- factor(df$status, levels=status_levels, ordered=TRUE)



positions <- c(0.5, -0.5, 1.0, -1.0, 1.5, -1.5)
directions <- c(1, -1)

line_pos <- data.frame(
  "date"=unique(df$date),
  "position"=rep(positions, length.out=length(unique(df$date))),
  "direction"=rep(directions, length.out=length(unique(df$date)))
)

df <- merge(x=df, y=line_pos, by="date", all = TRUE)
#df <- df[with(df, order(date, status)), ]

head(df)


#If there are multiple milestones for a given month, we need to slightly alter their positions (slightly higher if above our timeline and slightly lower if below our timeline).
#We can do a cumulative count of individual dates to check if we have multiple milestones for a given month.

text_offset <- 0.05

df$month_count <- ave(df$date==df$date, df$date, FUN=cumsum)
df$text_position <- (df$month_count * text_offset * df$direction) + df$position
#head(df)

month_buffer <- 12
year_buffer <- 0
year_date_range <- seq(min(df$date) - months(month_buffer), max(df$date) + months(month_buffer), by='year')
year_date_range <- as.Date(
  intersect(
    ceiling_date(year_date_range, unit="year"),
    floor_date(year_date_range, unit="year")
  ),  origin = "1970-01-01"
)
year_format <- format(year_date_range, '%Y')
year_df <- data.frame(year_date_range, year_format)




# Plot --------------------------------------------------------------------



timeline_plot<-ggplot(df,aes(x=date,y=0, 
                             #col=status, 
                             label=milestone)) +
           #labs(col="Milestones")+
           #scale_color_manual(values=status_colors, labels=status_levels, drop = FALSE)+
           theme_classic()+
           geom_hline(yintercept=0, 
                          color = "black", size=0.3)+
           geom_segment(data=df, aes(y=position,yend=0,xend=date), color='black', size=0.3)+
           geom_point(aes(y=0), size=3)+
           theme(axis.line.y=element_blank(),
                                   axis.text.y=element_blank(),
                                   axis.title.x=element_blank(),
                                   axis.title.y=element_blank(),
                                   axis.ticks.y=element_blank(),
                                   axis.text.x =element_blank(),
                                   axis.ticks.x =element_blank(),
                                   axis.line.x =element_blank(),
                                   legend.position = "bottom")+
           geom_text(data=year_df, aes(x=year_date_range,y=-0.2,label=year_format, 
                                       fontface="bold"),size=2.5, color='grey30', angle=90)+
          geom_text(aes(y=text_position,label=milestone),size=5)


print(timeline_plot)
