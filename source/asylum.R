library(ggplot2)
library(ggthemes)

asylum <- read.csv("./data/asylum.csv", stringsAsFactors = FALSE)
asylum$Date <- as.Date(asylum$Date)

cols <- c("asylum applications"="indianred3","asylum seekers (EASY system)"="steelblue","asylum seekers (EASY system, successor)"="dodgerblue4")

ggplot(asylum, aes(x = Date))+
  geom_line(aes(y = Erstantrag, col = "asylum applications")) + 
  geom_line(aes(y = EASY.Asylsuchende, col = "asylum seekers (EASY system)")) + 
  geom_line(aes(y = Asylsuchende, col = "asylum seekers (EASY system, successor)")) + 
  scale_x_date(date_breaks = "1 year", date_minor_breaks = "3 month", date_labels = "%Y", expand = c(0.0, 365/4)) + 
  scale_colour_manual(name="",values=cols) + 
  labs(x = "Date", y = "N") + 
  theme_hc() + 
  scale_y_continuous(labels = scales::comma) + 
  guides(color = guide_legend(nrow = 2, byrow = TRUE)) + 
  theme(legend.title = element_blank(), 
        panel.grid.minor.x = element_line(color = "lightgray"), 
        panel.grid.major.x = element_line(color = "lightgray"), 
        legend.text = element_text(size = 11))

