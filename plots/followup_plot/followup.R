library(epiCata)
require(tidyverse)
require(ggrepel)
require(ggplot2)
require(scales)
require(lubridate)

deaths <- read_csv("deaths_c.csv")
deaths$date <- ymd(deaths$date)

if(file.exists("models.csv")) {
  models <- read.csv("models.csv") %>% 
    mutate(date = ymd(date), model_date = ymd(model_date))
} else {
  models_filenames = list()
  for(dirpath in list.dirs("./results")[-1]){
    for(fname in list.files(dirpath, full.names = TRUE, include.dirs = FALSE, no.. = TRUE)){
      models_filenames <- append(models_filenames, fname)
    }
  }
  models_filenames <- unlist(models_filenames)

  # Load latest model
  models = NULL

  for(fname in models_filenames){
    cat("Loading file", fname)
    load(fname)

    fdf <- epiCata::get_merged_forecast_dfs(model_output$stan_list$available_locations, model_output, forecast=30, aggregate_name="SC_ESTADO")$data_location_forecast %>%
        rename(
          date=time,
          dmd=estimated_deaths_forecast,
          dlo=death_min_forecast,
          dhi=death_max_forecast
        ) %>%
        mutate(
          dmd = ifelse(date == min(date), 0, dmd),
          dlo = ifelse(date == min(date), 0, dlo),
          dhi = ifelse(date == min(date), 0, dhi)
        ) %>%
        left_join(deaths) %>%
        mutate(
          md = min(deaths_c),
          lo = min(deaths_c),
          hi = min(deaths_c)
        )

    dates <- sort(fdf$date)
    
    for(d in dates){
      ddf <- fdf[which(fdf$date == d),]
      d_dmd = ddf$dmd[[1]]
      d_dlo = ddf$dlo[[1]]
      d_dhi = ddf$dhi[[1]]

      fdf <- fdf %>%
        mutate(
          md = ifelse(date < d, md, md + d_dmd),
          lo = ifelse(date < d, lo, lo + d_dlo),
          hi = ifelse(date < d, hi, hi + d_dhi)
        )
    }
    
    fdf$model = ifelse(model_output$model_name=="base-reported","base-ron",model_output$model_name)
    fdf$model_date = min(fdf$date)
    models <- bind_rows(models, fdf)
  }

  write.csv(models, "models.csv")
}

date_start <- min(models$date)
date_end <- max(models$date)

followup <- deaths %>%
  filter(date_start<=date,date<=date_end)

wong_colours <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#CC79A7", "#0072B2",
                  "#D55E00", "#F0E442")
names(wong_colours) <- c("black", "orange", "sky blue", "bluish green", "reddish purple", "blue",
                  "vermillion", "yellow")

color_deaths <- wong_colours[["blue"]]
color_lo <- wong_colours[["yellow"]]
color_md <- wong_colours[["orange"]]
color_hi <- wong_colours[["vermillion"]]
color_vals <- c(color_deaths,color_lo,color_md,color_hi)
names(color_vals) <- c("Reported Deaths", "Scenario 1", "Scenario 2", "Scenario 3")
linetype_vals <- c("solid","dashed","dashed","dashed")
names(linetype_vals) <- c("Reported Deaths", "Scenario 1", "Scenario 2", "Scenario 3")



x_breaks_by <- "weeks"
x_breaks <- seq(date_start, date_end,by=x_breaks_by)
min_y <- round((min(models$deaths_c) + min(models$lo))/100)*100
max_y <- round_y_breaks(max(models$hi) + max(models$deaths_c))
y_scale <- (max_y - min_y)/4
y_breaks <- seq(min_y, max_y, y_scale)

print(unique(models$model))

p <- ggplot(models, mapping=aes(x=date)) +
  # Axis and Labels
  xlab("Date") +
  ylab("Cumulative deaths\n") +
  scale_x_date(labels = date_format("%e %b"),
               breaks=x_breaks,
               limits=c(date_start - days(1), date_end)) +
  #scale_y_continuous(limits=c(min(y_breaks), max(y_breaks)), breaks=y_breaks) +
  scale_y_continuous(breaks=pretty_breaks()) +
  # Deaths
  geom_line(data=followup,
            aes(y=deaths_c, color="Reported Deaths", label="Reported Deaths"),
            size=1, linetype="solid") +
  geom_point(data=followup %>% filter(date %in% x_breaks),
             aes(y=deaths_c, color="Reported Deaths"),
             alpha=1, size=2) +
  # Low
  geom_line(data=models, aes(y=lo, group=model_date, color="Scenario 1", label="Scenario 1"),
            size=0.5, alpha=0.5, linetype="dashed") +
  #geom_label_repel(data=models %>% filter(date == model_date+7),
  #                 aes(y=lo, group=model_date, color="Scenario 1"),#, label="Caso 1"),
  #                 size=4, show.legend = FALSE)  +
  geom_point(data=models %>% filter((date %in% x_breaks)),
             aes(y=lo, color="Scenario 1"),
             alpha=1, size=2) +
  # Medium
  geom_line(data=models,
            aes(y=md, group=model_date, color="Scenario 2", label="Scenario 2"),
            size=0.5, alpha=0.5, linetype="dashed") +
  #geom_label_repel(data=models %>% filter(date == model_date+7),
  #                 aes(y=md, group=model_date, color="Scenario 2"),#, label="Caso 2"),
  #                 size=4, show.legend = FALSE)  +
  geom_point(data=models %>% filter((date %in% x_breaks)),
             aes(y=md, color="Scenario 2"),
             alpha=1, size=2) +
  # High
  geom_line(data=models,
            aes(y=hi, group=model_date, color="Scenario 3", label="Scenario 3"),
            size=0.5, alpha=0.5, linetype="dashed") +
  #geom_label_repel(data=models %>% filter(date == model_date+7),
  #                 aes(y=hi, group=model_date, color="Scenario 3"),#, label="Caso 3"),
  #                 size=4, show.legend = FALSE) +
  geom_point(data=models %>% filter((date %in% x_breaks)),
             aes(y=hi, color="Scenario 3"),
             alpha=1, size=2) +
  # Training
  #geom_vline(xintercept=(models %>% filter(date == model_date))$date, color="#ED297C") +
  # Theme
  theme_dsb_light() +
  theme(panel.grid.major.x = element_line(linetype = "dotted", color = "grey", size=0.4),
        legend.text = element_text(margin = margin(l=2.5, t=0)),
        legend.title = element_text(margin=margin(t=0))) +
  scale_color_manual(name="Scenarios", values=color_vals) +
  scale_fill_manual(name="Scenarios", values=color_vals) +
  scale_linetype_manual(name="Scenarios", values=linetype_vals) +
  facet_wrap("model", nrow=2, ncol=1)
p

plot_filename <- "followup_weekly.pdf"
cat(sprintf("\n   Saving %s", plot_filename))
ggsave(file= plot_filename, p, width = 20, height=15, unit="cm")