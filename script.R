library(gganimate)
library(tidyverse)
library(RColorBrewer)
library(readxl)
library(ggnewscale)
library(ggpmisc)

setwd("~/height script")

df <- read_csv("NCD_RisC_Lancet_2020_height_child_adolescent_country.csv")
region <- read_csv("country_list2014.csv")

who_growth_standards <- list("Boys" = NA,
                             "Girls" = NA)

who_growth_standards <- who_growth_standards %>% imap(~ {
  
  exp <- read_excel(paste0("hfa-", .y, "-z-who-2007-exp.xlsx"))
  
  exp %>% 
    mutate(across("Month", ~ .x/12)) %>% 
    select(Month, SD2neg, SD1neg, SD0, SD1) %>% 
    pivot_longer(cols = c("SD2neg", "SD1neg", "SD0", "SD1"),
                 names_to = "curve",
                 values_to = "height")
  
})

who_growth_standards <- who_growth_standards %>% bind_rows(.id = "Sex") %>% rename(`Age group` = Month)

df <- df %>% left_join(region %>% select(Country, Superregion), by = "Country") %>% mutate(across("Superregion", as_factor))

df_sex <- list(Boys = NA,
               Girls = NA)

df_sex <- df_sex %>% imap(~ {
  
  int_df <- df %>% filter(Sex == .y)
  
  who_filter <- who_growth_standards %>% filter(Sex == .y, `Age group` == 19)
  
  pull_who_param <- function(unique_curve) who_filter %>% filter(curve == unique_curve) %>% pull(height)
  
  left_join(int_df, int_df %>%
              filter(`Age group` == 19) %>%
              mutate(
                Below2SD = case_when(`Mean height` <= pull_who_param("SD2neg") ~ TRUE, TRUE ~ FALSE),
                Below1SD = case_when(`Mean height` > pull_who_param("SD2neg") & `Mean height` <= pull_who_param("SD1neg") ~ TRUE, TRUE ~ FALSE),
                Between1and1SD = case_when(`Mean height` > pull_who_param("SD1neg") &
                                             `Mean height` < pull_who_param("SD1") ~ TRUE, TRUE ~ FALSE),
                Above1SD = case_when(`Mean height` >= pull_who_param("SD1") ~ TRUE, TRUE ~ FALSE)
              ) %>% 
              group_by(Year) %>% 
              summarise(across(contains("SD"), ~ sum(.x))), "Year")
  
})

df <- bind_rows(df_sex)

dimension_multiplier <- 1.1

# Cairo::CairoWin()
animations <- df %>% 
  ggplot() +
  theme_bw() +
  geom_line(aes(`Age group`, `Mean height`, colour = Superregion, group = Country), alpha = 0.5) +
  geom_line(data = who_growth_standards %>% mutate(across(curve, as_factor)) %>% mutate(across(curve, fct_rev)), 
            mapping = aes(x = `Age group`,
                          y = height,
                          group = curve,
                          linetype = curve,
                          size = curve,
                          alpha = curve
                          )) +
  scale_colour_manual(values = brewer.pal(n=12, "Paired")[c(7, 2, 6, 10, 1, 5, 4, 8, 12)]) +
  scale_x_continuous(breaks = seq(5, 19, 1)) +
  scale_y_continuous(breaks = scales::breaks_pretty(10)) +
  scale_linetype_manual(labels = c("1 SD/z-score", "Median/50th Percentile", "-1 SD/z-score", "-2 SD/z-score"),
                 values = c("dotted", "solid", "dotdash", "longdash")) +
  scale_size_manual(values = c(1, 1.5, 1, 1), guide = FALSE) +
  scale_alpha_manual(values = c(1, 0.66, 1, 1), guide = FALSE) +
  geom_text_npc(
    aes(
      npcx = 0.95,
      npcy = 0.075,
      label = as.character(.data[["Year"]])
    ), 
    data = . %>% filter(Country == "Afghanistan", `Age group` == 5),
    # colour = "white",
    size = 30*dimension_multiplier,
    show.legend = FALSE, 
    alpha = 0.3,
    hjust = 1
  ) +
  geom_text_npc(
    aes(
      npcx = 0.95,
      npcy = 0.2,
      label = paste0("Number of countries at\n19 years of age:\nBelow -2 SD: ",
                     .data[["Below2SD"]],
                     "\nBelow -1 SD: ",
                     .data[["Below1SD"]],
                     "\nBetween -1 and 1 SD: ",
                     .data[["Between1and1SD"]],
                     "\nAbove 1 SD: ",
                     .data[["Above1SD"]])
    ), 
    data = . %>% filter(Country == "Afghanistan", `Age group` == 5),
    hjust = 1,
    # colour = "white",
    alpha = 0.25,
    size = 10*dimension_multiplier,
    show.legend = FALSE,
    lineheight = .9
  ) +
  theme(
    text = element_text(family = "Segoe UI"),
    legend.position = "bottom", 
    legend.box = "vertical", 
    title = element_text(size = 48*dimension_multiplier),
    legend.text = element_text(size = 14*dimension_multiplier),
    legend.title = element_text(size = 14*dimension_multiplier), 
    axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0), size = 23*dimension_multiplier),
    axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0), size = 23*dimension_multiplier),
    axis.text = element_text(size = 20*dimension_multiplier),
    plot.caption = element_text(size = 14*dimension_multiplier), 
    strip.text.x = element_text(size = 23*dimension_multiplier)
  ) +
  labs(y = "Mean height (cm)"
       ,
       # title = "Global Height Trajectories of School-aged Children and Adolescents\nfor 1985 to 2019", 
       caption = "Data extracted from NCD Risk Factor Collaboration (NCD-RisC) | https://ncdrisc.org\nDOI: https://doi.org/10.1016/S0140-6736(20)31859-6\n* For the purpose of hierarchical analysis, countries were organised into 21 regions, mostly based on geography and national income. Regions were in turn organised into nine super-regions.\nTwitter: @VictorGYu"
       ) +
  guides(colour = guide_legend(title = "Super-region*", override.aes = list(alpha = 1, size = 2),
                               nrow = 3), 
         linetype = guide_legend(title = "2007 WHO Growth Reference Curves")) +
  facet_wrap(~ Sex, scales = "free_y") + coord_cartesian(clip = "off") +
  transition_time(Year)

animate(
  animations,
  nframes = 325,
  fps = 25,
  width = 1920,
  height = 1080,
  type = "cairo",
  start_pause = 50,
  end_pause = 50,
  renderer = gifski_renderer()
)
