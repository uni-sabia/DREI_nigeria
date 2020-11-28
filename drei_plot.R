library(dplyr)
library(ggplot2)
library(ggrepel)
library(ggthemes)

drei <- read.csv("drei.csv", stringsAsFactors=FALSE) 
names(drei)
str(drei)
drei <- drei %>% mutate(Risk = as.factor(Risk)) %>% 
  mutate(Risk=reorder(Risk, Rank_plot)) %>% 
  group_by(Rank)
# Reorder Legend to show the order of risk
drei$Risk <- factor(drei$Risk, levels=rev(levels(drei$Risk)))

all <- ggplot(drei, aes(x=Likelihood, y=Impact, label=Risk_Sub)) + 
  geom_point(aes(size=Rank_plot, color=Risk)) +
  theme_minimal(base_size=15) +
  geom_text_repel(size=4.5, nudge_y=0.09) +
  # labs(col="Ranking of \nRisk Categories \n(in descending order)", size=5) +
  guides(size=FALSE) +
  theme(legend.position="none")
all
ggsave("risk_plot.png", width=13, height=10)

category <- ggplot(drei, aes(x=Likelihood, y=Impact, label=Risk_Sub)) + 
  geom_point(aes(size=Rank_plot, color=as.numeric(Risk)), alpha=0.6) +
  facet_wrap(~Risk) + 
  scale_color_continuous() +
  geom_text_repel(size=2.7, 
                  point.padding=NA, nudge_x=0.15, nudge_y=0.15) +
  theme_stata() +
  theme(legend.position="none")
category
ggsave("risk_plot_facet.png", width=10, height=8)

