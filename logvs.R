library(tidyverse)
system.time({
  source('/Users/jackliu/Desktop/source_data_entry.local.R')
}) 

# user 29.779 + system 6.306 = elapsed 38.273

ggplot(mortality_vis, aes(x = id) ) +
  geom_line(lwd = 1.5, alpha = 0.7, aes(y = Smooth, colour = Series)) +
  geom_col(alpha = 0.3, position = "identity", aes(y = `Due to`, colour = Series))



+
  labs(x="Week", y="Number of ppl",
       title="Registered deaths due to influenza and pneumonia",
       caption="As reported by ONS for England and Wales") +
  theme_ipsum() + 
  scale_x_continuous(breaks = seq(0, 34, 2), 
                     minor_breaks = seq(0, 34, 1),
                     labels = c("40", "42", "44", "46", "48",
                                "50", "52", "2", "4", "6", "8", 
                                "10", "12", "14", "16", "18",
                                "20", "22"))


  scale_y_continuous(breaks = seq(0, 60, 10), minor_breaks = seq(0, 60, 5)) +
  theme(panel.border = element_rect(color = "dark grey", fill = NA, lwd = 0.1)) +
  geom_ribbon(aes(ymin=0,ymax=12.7,fill="#B7CE89"), alpha=0.25) +
  geom_ribbon(aes(ymin=12.7,ymax=24.1,fill="#FEFF67"), alpha=0.25)+
  geom_ribbon(aes(ymin=24.1,ymax=60,fill="black"),  alpha=0.25)+
  scale_color_manual('Season', values= wes_palette("Moonrise1", n = 4)) +
  coord_cartesian(ylim = c(0, 60), expand = FALSE) +
  scale_fill_manual(values=c("#B7CE89","#FEFF67","#F7B27E"), name="The MEM threshold",
                    labels = c("Baseline threshold", "Low", "Medium"),
                    guide = guide_legend(reverse = F, order = 2))
mortplot




mortality_vis %>%
  rename("Dueto" = "Due to") %>%
  arrange(Series, id) %>% 
  mutate(t2=lag(Dueto),
         t1=lag(Dueto,2),
         mova=(Dueto + t1 + t2)/3) %>%
ggplot(aes(x = id, y = `mova`) ) +
    #geom_smooth(lwd = 1.5, aes(colour = `Series`))
  geom_line(lwd = 1.5, aes(colour = `Series`)) 

# There was an attempt -----
# `mova` as in moving average
# `past`    as in how many previous rows do we consider
# `incl` as in do we include the current row (T/F)

mova <- function(data, col, past, incl=F) {
  col = enquo(col)
  data %>%
    for(i in past:0)
      mutate("lag{past}" := lag(!! col, past)) %>%
  return()
}
# Using Rcpp Roll ----

mortality_vis %>%
  mutate(mova = roll_meanr(`Due to`, n=3))

