library(tidyverse)
library(lme4)
library(broom)
library(CalledStrike)
library(ggrepel)

unique(TM_Update$Type)
unique(TM_Update$Count)
unique(TM_Update$BatterSide)
unique(TM_Update$PitcherThrows)

## Filter Trackman data to just be balls in play that aren;t bunts
TM_Update_Platoon <- TM_Update %>% 
  filter(
    Level == "D1", # D1 level games
    PitcherThrows %in% c("Left", "Right"), # no weird errors
    BatterSide %in% c("Left", "Right"), # no weird errors
    woba_denom == 1 # only the final pitch of the PA
  )  %>%
  mutate(
    Platoon_Advantage = if_else(PitcherThrows == BatterSide, "Same Side", "Oppo Side") # add a variable for if the batter had the same or oppo platoon
  ) %>%
  select(Batter, BatterTeam, Platoon_Advantage, woba_value, woba_denom) # select only necessary columns

## Filter DF to just players with at least 150 TM PAs
N_PA <- TM_Update_Platoon %>% 
  group_by(Batter, BatterTeam) %>%
  summarize(
    PA = n(),
    Same_Side_PA = sum(woba_denom[Platoon_Advantage == "Same Side"], na.rm = T),
    Oppo_Side_PA = sum(woba_denom[Platoon_Advantage == "Oppo Side"], na.rm = T)
  ) # number of PAs

TM_Update_Platoon_Filtered <- inner_join(TM_Update_Platoon, N_PA, by= c("Batter", "BatterTeam")) %>%
  filter(
    PA >= 150,
    Same_Side_PA >= 35, 
    Oppo_Side_PA >= 35
  ) # only want players with at least 150 trackman plate appearances and at least 35 PAs from both split

rm(TM_Update_Platoon, N_PA) # remove the old DFs we don't need

## Create DF with players wOBA 
Batter_wOBA <- TM_Update_Platoon_Filtered %>% 
  group_by(Batter, BatterTeam, Platoon_Advantage) %>%
  summarize(
    PA = n(),
    wOBA = mean(woba_value, na.rm = TRUE)
  )

## Put the platoon side splits in the same df on the same column
Batter_wOBA_Combined <- inner_join(
  filter(Batter_wOBA, Platoon_Advantage == "Oppo Side"),
  filter(Batter_wOBA, Platoon_Advantage == "Same Side"),
  by = c("Batter", "BatterTeam")
)

Batter_wOBA_Combined <- Batter_wOBA_Combined %>%
  mutate(
    Diff = wOBA.x - wOBA.y,
    Same_Higher = if_else(wOBA.y > wOBA.x, T, F),
    Oppo_Higher = if_else(wOBA.x > wOBA.y, T, F),
    Which_Side = if_else(wOBA.y > wOBA.x, "Same Higher",
                         if_else(wOBA.x > wOBA.y, "Oppo Higher", NA))
  )

Batter_wOBA_Combined %>%
  group_by(Which_Side) %>%
  summarise(
    Count = n()
  ) %>%
  ggplot(aes(x = Which_Side, y = Count, fill = Which_Side)) +
  geom_bar(stat = "identity") +
  labs(
    title = "Comparing Same Side and Oppo Side PA Advantages",
    subtitle = "2023 Season min. 150 Trackman PA \n & 35 Same Side and Oppo Side PA",
    y = "N",
    x = "Platoon Advantage",
    fill = "wOBA by Platoon Split PA"
  ) +
  theme_bw() +
  theme(plot.title = element_text(colour = "black", size = 20, face = "bold",
                                  hjust = 0.5, vjust = 0.8, angle = 0),
        plot.subtitle = element_text(colour = "black", size = 14,
                                     hjust = 0.5, vjust = 0.8, angle = 0),
        text=element_text(size=14),
        legend.position = "bottom",
        panel.border = element_rect(fill = NA, colour = "black", linewidth = 1.5),
        axis.title = element_text(face = "bold"),
        strip.text = element_text(colour = "black", size = 13, face = "bold"))
  

mean(Batter_wOBA_Combined$Diff)
head(Batter_wOBA_Combined)
rm(Batter_wOBA)

## Create plot showing xwOBACON improvement in Hitters Counts Compared to 2 Strike Counts
Batter_wOBA_Combined %>%
  mutate(OppoSide_minus_SameSide = wOBA.x - wOBA.y) %>%
  arrange(desc(OppoSide_minus_SameSide)) %>%
  ggplot() +
  geom_point(
    aes(
      x = (wOBA.x + wOBA.y)/2, 
      y = (wOBA.x - wOBA.y)
    )
  ) +
  geom_point(
    data = Batter_wOBA_Combined %>% filter(BatterTeam == "CAL_FUL"),
    aes(
      x = (wOBA.x + wOBA.y)/2, 
      y = (wOBA.x - wOBA.y)
    ),
    color = "orange",
    size = 5
  ) +
  geom_text_repel(
    data = Batter_wOBA_Combined %>% filter(BatterTeam == "CAL_FUL"),
    aes(
      x = (wOBA.x + wOBA.y)/2, 
      y = (wOBA.x - wOBA.y),
      label = Batter
    )
  ) +
  geom_hline(yintercept = 0, color="orange", lwd = 1) +
  labs(
    title = "Comparing Same Side and Oppo Side PA wOBA",
    subtitle = "2023 Season min. 150 Trackman PA \n & 35 Same Side and Oppo Side PA",
    y = "Improvement in Oppo Side wOBA",
    x = "wOBA"
  ) +
  theme_bw() +
  theme(plot.title = element_text(colour = "black", size = 20, face = "bold",
                                  hjust = 0.5, vjust = 0.8, angle = 0),
        plot.subtitle = element_text(colour = "black", size = 14,
                                     hjust = 0.5, vjust = 0.8, angle = 0),
        text=element_text(size=14),
        legend.position = "bottom",
        panel.border = element_rect(fill = NA, colour = "black", linewidth = 1.5),
        axis.title = element_text(face = "bold"),
        strip.text = element_text(colour = "black", size = 13, face = "bold"))

## Add the effect variable for oppo side and same side platoon PA
TM_Update_Platoon_Filtered <- TM_Update_Platoon_Filtered %>%
  mutate(
    platoon_effect = if_else(Platoon_Advantage == "Oppo Side", 1, 0)
  )

## Create the linear regression model
lm_fit <- lm(woba_value ~ platoon_effect, data = TM_Update_Platoon_Filtered)
lm_fit$coefficients
summary(lm_fit)

## Graph the linea regression estimates
lm_date <- data.frame(
  Platoon_Advantage = c(
    "Same Side", "Oppo Side"
  ),
  pred = c(
    lm_fit$coefficients[1] - lm_fit$coefficients[2] / 2,
    lm_fit$coefficients[1] + lm_fit$coefficients[2] / 2
  )
)

lm_date <- lm_date %>%
  mutate(
    Group = "Linear"
  )

ggplot(lm_date, aes(x = Platoon_Advantage, y = pred, group = Group)) +
  geom_point(size = 3) + 
  geom_line(lwd = 1) +
  ylim(0.2, 0.6) + 
  labs(
    title = "Regression Curve for Platoon Advantages",
    subtitle = "2023 Season min. 150 Trackman PA \n & 35 Same Side and Oppo Side PA",
    y = "Predicted wOBA",
    x = "Platoon Advantage"
  ) +
  theme_bw() +
  theme(plot.title = element_text(colour = "black", size = 20, face = "bold",
                                  hjust = 0.5, vjust = 0.8, angle = 0),
        plot.subtitle = element_text(colour = "black", size = 14,
                                     hjust = 0.5, vjust = 0.8, angle = 0),
        text=element_text(size=14),
        legend.position = "bottom",
        panel.border = element_rect(fill = NA, colour = "black", linewidth = 1.5),
        axis.title = element_text(face = "bold"),
        strip.text = element_text(colour = "black", size = 13, face = "bold"))


## Create a multilevel model using the batter as a random effect
multilevel_fit <- lmer(woba_value ~ platoon_effect + (1 | Batter), data = TM_Update_Platoon_Filtered)
summary(multilevel_fit)

multilevel_model_df <- coef(multilevel_fit)[[1]]
names(multilevel_model_df) <- c(
  "beta0", "beta1"
)
multilevel_model_df$Batter <- row.names(multilevel_model_df)
row.names(multilevel_model_df) <- NULL

multilevel_model_df %>%
  mutate(
    `Same Side` = beta0 - beta1 / 2,
    `Oppo Side` = beta0 + beta1 / 2
  ) %>%
  as_tibble() %>%
  select(Batter, `Same Side`, `Oppo Side`) %>%
  gather(Platoon_Advantage, wOBA, -Batter) %>%
  ggplot(aes(Platoon_Advantage, wOBA, group = Batter)) +
  geom_line() +
  geom_point() +
  labs(
    title = "Multilevel Regression Curves",
    subtitle = "2023 Season min. 150 Trackman PA \n & 35 Same Side and Oppo Side PA",
    y = "Predicted wOBA",
    x = "Platoon Advantage"
  ) +
  ylim(.2, .6) +
  theme_bw() +
  theme(plot.title = element_text(colour = "black", size = 20, face = "bold",
                                  hjust = 0.5, vjust = 0.8, angle = 0),
        plot.subtitle = element_text(colour = "black", size = 14,
                                     hjust = 0.5, vjust = 0.8, angle = 0),
        text=element_text(size=14),
        legend.position = "bottom",
        panel.border = element_rect(fill = NA, colour = "black", linewidth = 1.5),
        axis.title = element_text(face = "bold"),
        strip.text = element_text(colour = "black", size = 13, face = "bold"))

## the multilevel model is predicting a difference of .019 wOBA between oppo side and same side platoon plate appearances
