source("dsb_ggplot_theme.R")
require(tidyverse)
require(lubridate)
require(scales)

dsb_colours <- c("#ED297C", "#008FD5", "#FF2700", "#FFAB40", "#810F7C", "#619656",
             "#11C8ED", "#ED5434", "#11ED98", "#C61EF7", "#78342A", "#FC5900",
             "#AB5F00", "#414D91", "#032738", "#193AF7", "#243820")
wong_colours <- c("#E69F00", "#56B4E9", "#009E73", "#CC79A7", "#0072B2",
                  "#D55E00", "#000000", "#F0E442")
tol_colours <- c("#332288", "#117733", "#44AA99", "#88CCEE", "#DDCC77", "#CC6677",
                  "#AA4499", "#882255")
colours <- wong_colours

colors <- colours

data <- read.csv("onset_to_death.csv") %>% rename(onset_to_death = data_onset_death)
data$dat_obito <- ymd(data$dat_obito)
data_filtered <- data %>% filter(onset_to_death>0)

print("Não filtrado")
print(length(data$onset_to_death))
print(length(filter(data, onset_to_death<=0)$onset_to_death))
print(mean(data$onset_to_death))
print(sd(data$onset_to_death))
print(sd(data$onset_to_death)/mean(data$onset_to_death))

print("Filtrado")
print(length(data_filtered$onset_to_death))
print(mean(data_filtered$onset_to_death))
print(sd(data_filtered$onset_to_death))
print(sd(data_filtered$onset_to_death)/mean(data_filtered$onset_to_death))

s_d <- data %>% group_by(dat_obito) %>% summarize(avg_onset_to_death=mean(onset_to_death))
s_d_f <- data_filtered %>% group_by(dat_obito) %>% summarize(avg_onset_to_death=mean(onset_to_death))

g <- ggplot() +
  geom_line(data=s_d, aes(x = dat_obito, y=avg_onset_to_death), color=colors[[1]]) +
  geom_line(data=s_d_f, aes(x = dat_obito, y=avg_onset_to_death), color=colors[[2]]) + 
  scale_x_date(name="", breaks="2 weeks") +
  scale_y_continuous(name="Average Onset-to-Death") +
  theme_dsb_light()
ggsave("onset-to-death-timeline.png", g,
       width=7, height=4, dpi=150)
ggsave("onset-to-death-timeline.pdf", g,
       width=20, height=10, unit="cm")


### SIMPLE

myhist1 <- function(data, fill, binwidth=5, line_color=1, line_width=1, line_type="solid") {
  avg <- mean(data$onset_to_death)
  g <- ggplot(data) +
    geom_histogram(aes(x = onset_to_death), binwidth=binwidth, color=1, fill=fill) +
    scale_x_continuous(breaks=seq(0,max(data$onset_to_death),binwidth*2), name="Onset-to-death") +
    scale_y_continuous(name="Count") +
    geom_vline(xintercept = avg, size=line_width, colour=line_color, linetype=line_type) +
    theme_dsb_light()
}

g <- myhist1(data, colors[[1]]) +
  ggtitle("")
ggsave("hist1_full.png", g,
       width=7, height=4, dpi=150)
ggsave("hist1_full.pdf", g,
       width=20, height=10, unit="cm")

g <- myhist1(data_filtered, colors[[2]]) +
  ggtitle("") #ggtitle("(Ignoring values ≤0)")
ggsave("hist1_filtered.png", g,
       width=7, height=4, dpi=150)
ggsave("hist1_filtered.pdf", g,
       width=20, height=10, unit="cm")

### STRONGER LINE

g <- myhist1(data, colors[[1]], line_color=colors[[3]], line_width=3, line_type="dashed") +
  ggtitle("")
ggsave("hist2_full.png", g,
       width=7, height=4, dpi=150)
ggsave("hist2_full.pdf", g,
       width=20, height=10, unit="cm")

g <- myhist1(data_filtered, colors[[2]], line_color=colors[[3]], line_width=3, line_type="dashed") +
  ggtitle("") #ggtitle("(Ignoring values ≤0)")
ggsave("hist2_filtered.png", g,
       width=7, height=4, dpi=150)
ggsave("hist2_filtered.pdf", g,
       width=20, height=10, unit="cm")

### Bar on mean

myhist3 <- function(data, fill, binwidth=5) {
  maxsize <- max(hist(data$onset_to_death, breaks=seq(min(data$onset_to_death), max(data$onset_to_death)+binwidth, binwidth), plot=FALSE)$counts)
  avg <- mean(data$onset_to_death)
  g <- ggplot(data) +
    geom_histogram(aes(x = onset_to_death), binwidth=binwidth, color=1, alpha=.6) +
    scale_x_continuous(breaks=seq(0,max(data$onset_to_death),binwidth*2), name="Onset-to-death") +
    scale_y_continuous(name="Count") +
    geom_tile(aes(x=avg, y=maxsize/2, width=1, height=maxsize), fill=fill) +
    theme_dsb_light()
}

g <- myhist3(data, colors[[1]]) +
  ggtitle("")
ggsave("hist3_full.png", g,
       width=7, height=4, dpi=150)
ggsave("hist3_full.pdf", g,
       width=20, height=10, unit="cm")

g <- myhist3(data_filtered, colors[[2]]) +
  ggtitle("") #ggtitle("(Ignoring values ≤0)")
ggsave("hist3_filtered.png", g,
       width=7, height=4, dpi=150)
ggsave("hist3_filtered.pdf", g,
       width=20, height=10, unit="cm")

### Bar on mean + std

myhist4 <- function(data, fill, binwidth=5) {
  maxsize <- max(hist(data$onset_to_death, breaks=seq(min(data$onset_to_death), max(data$onset_to_death)+binwidth, binwidth), plot=FALSE)$counts)
  avg <- mean(data$onset_to_death)
  std <- sd(data$onset_to_death)
  g <- ggplot(data) +
    geom_tile(aes(x=avg, y=maxsize/2, width=2*std, height=maxsize), fill=fill, alpha=0.01)+
    geom_histogram(aes(x = onset_to_death), binwidth=binwidth, color=1, fill="#9B9B9B") +
    scale_x_continuous(breaks=seq(0,max(data$onset_to_death),binwidth*2), name="Onset-to-death") +
    scale_y_continuous(name="Count") +
    geom_tile(aes(x=avg, y=maxsize/2, width=1, height=maxsize), fill=fill) +
    theme_dsb_light()
}

g <- myhist4(data, colors[[1]]) +
  ggtitle("")
ggsave("hist4_full.png", g,
       width=7, height=4, dpi=150)
ggsave("hist4_full.pdf", g,
       width=20, height=10, unit="cm")

g <- myhist4(data_filtered, colors[[2]]) +
  ggtitle("") #ggtitle("(Ignoring values ≤0)")
ggsave("hist4_filtered.png", g,
       width=7, height=4, dpi=150)
ggsave("hist4_filtered.pdf", g,
       width=20, height=10, unit="cm")



data_fg <- data_filtered %>% group_by(semana=floor_date(dat_obito,"week"))

breaks <- seq(min(data_filtered$dat_obito), max(data_filtered$dat_obito), by="2 weeks")
g <- ggplot(data_fg) +
  geom_violin(aes(y = onset_to_death, x = semana, group=semana), fill=colors[[1]]) +
  scale_x_date(name="", labels = date_format("%e %b"), breaks=breaks, limits=c(min(breaks), max(breaks))) +
  scale_y_continuous("Onset-to-Death") +
  theme_dsb_light()
ggsave("weekly_violin_filtered.png", g,
       width=7, height=4, dpi=150)
ggsave("weekly_violin_filtered.pdf", g,
       width=20, height=10, unit="cm")
