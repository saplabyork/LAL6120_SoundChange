
andrew_data <- read.csv("/Users/chandannarayan/Desktop/Andrrew.csv", header = TRUE)
library(lme4)

library(dplyr)

andrew_data <- andrew_data %>% 
  mutate(sub = dense_rank(ID))

library(lmerTest)
library(dplyr)

#cog model
model_cog <- lmer(cog ~ seg * cond + (1|sub), data = andrew_data)
summary(model_cog)
anova(model_cog)

library(ggplot2)
library(emmeans)

# estimate means
emm_interaction <- emmeans(model_cog, ~ segment * condition)
emm_segment <- emmeans(model_cog, ~ segment)
emm_condition <- emmeans(model_cog, ~ condition)

# dataframes for plots
df_interaction <- as.data.frame(emm_interaction)
df_segment <- as.data.frame(emm_segment)
df_condition <- as.data.frame(emm_condition)

# Create a multi-panel plot
library(gridExtra)

cog_interaction <- ggplot(df_interaction, aes(x = segment, y = emmean, fill = condition)) +
  geom_bar(stat = "identity", position = "dodge", alpha = 0.7) +
  geom_errorbar(aes(ymin = emmean - SE, ymax = emmean + SE), 
                position = position_dodge(width = 0.9), width = 0.2) +
  labs(title = "Segment × Condition Interaction",
       x = "Segment", y = "Estimated Marginal Mean (cog)",
       fill = "Condition") +
  theme_minimal() +
  theme(legend.position = "bottom")
cog_interaction


# pairwise comparisons
s_comparison <- emmeans(model_cog, ~ condition | segment)
s_pairs <- pairs(s_comparison)
print("Pairwise comparisons within each segment:")
print(s_pairs)


###################################SKEWNESS################

#skewness model
model_skew <- lmer(skew ~ segment * condition + (1|sub), data = andrew_data)
summary(model_skew)
anova(model_skew)

library(ggplot2)
library(emmeans)

# marginal means
emm_interaction_skew <- emmeans(model_skew, ~ segment * condition)
emm_segment_skew <- emmeans(model_skew, ~ segment)
emm_condition_skew <- emmeans(model_skew, ~ condition)

# make dataframes for the plots
df_interaction_skew <- as.data.frame(emm_interaction_skew)
df_segment_skew <- as.data.frame(emm_segment_skew)
df_condition_skew <- as.data.frame(emm_condition_skew)

library(gridExtra)

skew_interaction <- ggplot(df_interaction_skew, aes(x = segment, y = emmean, fill = condition)) +
  geom_bar(stat = "identity", position = "dodge", alpha = 0.7) +
  geom_errorbar(aes(ymin = emmean - SE, ymax = emmean + SE), 
                position = position_dodge(width = 0.9), width = 0.2) +
  labs(title = "Segment × Condition Interaction",
       x = "Segment", y = "Estimated Marginal Mean (skewness)",
       fill = "Condition") +
  theme_minimal() +
  theme(legend.position = "bottom")
skew_interaction


# pairwise comparisons
s_comparison_skew <- emmeans(model_skew, ~ condition | segment)
s_pairs_skew <- pairs(s_comparison_skew)
print("Pairwise comparisons within each segment:")
print(s_pairs_skew)
