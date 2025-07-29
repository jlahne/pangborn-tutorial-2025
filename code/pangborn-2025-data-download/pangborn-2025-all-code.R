## ----setup---------------------------------------------------------------------------------------------------------------------------------
library(tidyverse)


## ----workshop-code, child = c("01-import-motivation-export.Rmd", "02-ggplot2-basics.Rmd", "03-finetuning-ggplot.Rmd", "04-ggplot2-extensions.Rmd", "05-conclusions.Rmd")----

## ----libraries-2---------------------------------------------------------------------------------------------------------------------------
# This package is actually a set of utility packages we will use a lot
library(tidyverse)


## ----get-working-directory-----------------------------------------------------------------------------------------------------------------
getwd()


## ----store-the-data, message=FALSE---------------------------------------------------------------------------------------------------------
raw_berry_data <- read_csv(file = "data/clt-berry-data.csv")
raw_cider_data <- read_csv(file = "data/CiderDryness_SensoryDATA.csv")


## ----cider CA final, message = FALSE, fig.align='center', fig.width = 7, results = FALSE---------------------------------------------------
raw_cider_data <- 
  read_csv("data/CiderDryness_SensoryDATA.csv")

cider_samples <-
  raw_cider_data %>%
  select(Sample_Name, Temperature) %>%
  unite(Sample_Name, Temperature, col = "sample", sep = " ", remove = FALSE) %>%
  distinct()

ca_cider <- 
  raw_cider_data %>%
  select(Sample_Name, Temperature, Fresh_Apples:Synthetic) %>%
  unite(Sample_Name, Temperature, col = "sample", sep = " ") %>%
  group_by(sample) %>%
  summarize(across(where(is.numeric), ~sum(.))) %>%
  column_to_rownames("sample") %>%
  FactoMineR::CA(graph = FALSE)

ca_cider_coords <- 
  ca_cider$row$coord %>%
  as_tibble(rownames = "name") %>%
  mutate(type = "row") %>%
  bind_rows(
    ca_cider$col$coord %>%
      as_tibble(rownames = "name") %>%
      mutate(type = "col")
  ) %>%
  left_join(cider_samples, join_by(name == sample)) %>%
  mutate(name = if_else(is.na(Sample_Name), name, Sample_Name),
         name = str_replace_all(name, "_", " "),
         name = str_replace(name, "FullBodied", "Full Bodied"))

nice_cider_labels <-
  labs(x = str_c("Dimension 1, ", round(ca_cider$eig[1, 2], 1), "% of inertia"),
       y = str_c("Dimension 2, ", round(ca_cider$eig[2, 2], 1), "% of inertia"),
       subtitle = "Correspondence Analysis biplot (symmetric)",
       title = "Effect of cider serving temperature on consumer sensory perception")

p2_ca_cider_cata <- 
  ca_cider_coords %>%
  mutate(font = if_else(type == "row", "plain", "italic")) %>%
  ggplot(aes(x = `Dim 1`, y = `Dim 2`)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "darkgrey") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "darkgrey") +
  geom_point(aes(color = type, shape = Temperature),
             data = ca_cider_coords %>% filter(type == "row"),
             size = 3) +
  ggrepel::geom_text_repel(aes(label = name, color = type, fontface = font),
                           show.legend = FALSE) +
  coord_equal() + 
  theme_linedraw() +
  theme(legend.position = "bottom") +
  nice_cider_labels + 
  scale_color_manual(values = c("darkorange", "darkgreen")) +
  scale_shape_manual(values = c(8, 16)) +
  guides(shape = guide_legend(),
         color = "none")

p2_ca_cider_cata


## ----berry-full-penalty, message=FALSE, fig.align='center', fig.width=7, fig.height=4, results=FALSE---------------------------------------
# Import the data
raw_berry_data <- 
  read_csv(file = "data/clt-berry-data.csv") %>%
  select(where(~ !all(is.na(.)))) 

cleaned_berry_data <-
  raw_berry_data %>%
  # Get the relevant columns
  select(`Subject Code`, 
         berry,
         sample,
         starts_with("cata_"), 
         contains("overall")) %>%
  # Rescale the LAM and US scales to a 9-pt range
  mutate(lms_overall = (lms_overall + 100) * (8 / 200) + 1,
         us_overall = (us_overall + 0) * (8 / 15) + 1) %>%
  # Switch the 3 overall liking columns into a single column
  pivot_longer(contains("overall"),
               names_to = "hedonic_scale",
               values_to = "rating",
               values_drop_na = TRUE) %>%
  # Let's make all the CATA variables into a single column to make life easier
  # (and get rid of those NAs)
  pivot_longer(starts_with("cata_"),
               names_to = "cata_variable",
               values_to = "checked",
               names_transform = ~str_remove(., "cata_"),
               values_drop_na = TRUE)

berry_penalty_analysis_data <- 
  cleaned_berry_data %>%
  group_by(berry, cata_variable, checked) %>%
  summarize(penalty_lift = mean(rating),
            count = n()) %>%
  ungroup() 

# Make a plot of the overall penalty/lift for checked attributes
p1_berry_penalty <- 
  berry_penalty_analysis_data %>%
  select(-count) %>%
  pivot_wider(names_from = checked,
              values_from = penalty_lift,
              names_prefix = "checked_") %>%
  group_by(berry, cata_variable) %>%
  summarize(penalty_lift = checked_1 - checked_0) %>%
  # We can tidy up our CATA labels
  separate(cata_variable, 
           into = c("mode", "variable"), 
           sep = "_") %>%
  # Fix a typo
  mutate(mode = str_replace(mode, "appearane", "appearance")) %>%
  mutate(mode = case_when(mode == "taste" ~ "(T)",
                          mode == "appearance" ~ "(A)")) %>%
  unite(variable, mode, col = "cata_variable", sep = " ") %>%
  # We are using a function from tidytext that makes faceting the final figure
  # easier
  mutate(cata_variable = tidytext::reorder_within(x = cata_variable,
                                                  by = penalty_lift,
                                                  within = berry)) %>%
  #And finally we plot!
  ggplot(mapping = aes(x = cata_variable, y = penalty_lift)) +
  geom_col(aes(fill = penalty_lift), color = "white", show.legend = FALSE) + 
  facet_wrap(~berry, scales = "free", nrow = 1) + 
  tidytext::scale_x_reordered() + 
  coord_flip() + 
  theme_classic() + 
  scale_fill_gradient(low = "tan", high = "darkgreen") + 
  labs(x = NULL, y = NULL,
       title = "Penalty / Lift Analysis",
       subtitle = "displays the mean difference (within berries) for when a CATA variable is checked\nor un-checked")

p1_berry_penalty


## ----CA biplot made by FactoMineR----------------------------------------------------------------------------------------------------------
p3_cider_factominer <- plot(ca_cider)
p3_cider_factominer


## ----editing the FactoMineR plot-----------------------------------------------------------------------------------------------------------
p3_cider_factominer +
  theme_dark() + 
  labs(caption = "Now we can say some more things!", 
       subtitle = "of 6 ciders tasted by 48 subjects")


## ----limits of automatic plots-------------------------------------------------------------------------------------------------------------
p3_cider_factominer + 
  scale_color_manual(values = c("darkorange", "darkgreen"))


## ----demonstrating write_csv---------------------------------------------------------------------------------------------------------------
# We will keep in the tidyverse idiom with readr::write_csv()
write_csv(x = berry_penalty_analysis_data,
          file = "data/berry-penalty-data.csv")


## ----demonstrating write_rds---------------------------------------------------------------------------------------------------------------
berry_penalty_analysis_data %>%
  write_rds(file = "data/berry-penalty-data.rds")


## ----saving multiple data objects together-------------------------------------------------------------------------------------------------
save(berry_penalty_analysis_data,
     ca_cider_coords,
     ca_cider,
     file = "data/workshop-data.RData")

rm(berry_penalty_analysis_data,
     ca_cider_coords,
     ca_cider)

load(file = "data/workshop-data.RData")


## ----non-working schematic of a ggplot, eval = FALSE---------------------------------------------------------------------------------------
## # The ggplot() function creates your plotting environment.  We usually save it to a variable in R so that we can use the plug-n-play functionality of ggplot without retyping a bunch of nonsense
## p <- ggplot(mapping = aes(x = <a variable>, y = <another variable>, ...),
##             data = <your data>)
## 
## # Then, you can add various ways of plotting data to make different visualizations.
## p +
##   geom_<your chosen way of plotting>(...) +
##   theme_<your chosen theme> +
##   ...


## ----a first ggplot------------------------------------------------------------------------------------------------------------------------
# We start with our data and pipe it into ggplot
ca_cider_coords %>%
  # Here we set up the base plot
  ggplot(mapping = aes(x = `Dim 1`, y = `Dim 2`)) + 
   # Here we tell our base plot to add points
  geom_point()                          


## ----what we are plotting in this example--------------------------------------------------------------------------------------------------
ca_cider_coords %>% 
  select(`Dim 1`, `Dim 2`)


## ----switching geom switches the way the data map------------------------------------------------------------------------------------------
ca_cider_coords %>%
  ggplot(mapping = aes(x = `Dim 1`, y = `Dim 2`)) + 
  # connecting our points with a line makes even less sense
  geom_line()


## ----geoms are layers in a plot------------------------------------------------------------------------------------------------------------
ca_cider_coords %>%
  ggplot(mapping = aes(x = `Dim 1`, y = `Dim 2`)) + 
  geom_point() +
  # add a layer with the names of the points
  geom_text(mapping = aes(label = name))


## ----here are some other parts of the plot we can control with data------------------------------------------------------------------------
ca_cider_coords %>%
  ggplot(mapping = aes(x = `Dim 1`, y = `Dim 2`, color = type)) + 
  geom_point() +
  geom_text(mapping = aes(label = name))


## ----color only one layer------------------------------------------------------------------------------------------------------------------
ca_cider_coords %>%
  ggplot(mapping = aes(x = `Dim 1`, y = `Dim 2`)) + 
  # we will only color the points, not the text
  geom_point(mapping = aes(color = type)) +
  geom_text(mapping = aes(label = name))


## ------------------------------------------------------------------------------------------------------------------------------------------
ca_cider_coords %>%
  ggplot(mapping = aes(x = `Dim 1`, y = `Dim 2`)) + 
  geom_point(mapping = aes(color = type)) +
  geom_text(mapping = aes(label = name),
            hjust = "outward",
            color = "darkblue")


## ----showing ggrepel-----------------------------------------------------------------------------------------------------------------------
ca_cider_coords %>%
  ggplot(mapping = aes(x = `Dim 1`, y = `Dim 2`)) + 
  geom_point(mapping = aes(color = type)) +
  # here we opt for not loading the package
  ggrepel::geom_text_repel(mapping = aes(label = name),
            color = "darkblue") +
  scale_color_manual(values = c("darkorange", "darkgreen"))


## ----adding a different theme--------------------------------------------------------------------------------------------------------------
ca_cider_coords %>%
  ggplot(mapping = aes(x = `Dim 1`, y = `Dim 2`)) + 
  geom_point(mapping = aes(color = type)) +
  ggrepel::geom_text_repel(mapping = aes(label = name),
            color = "darkblue") +
  # here we switch to a black and white theme
  theme_bw()


## ----using the theme functions-------------------------------------------------------------------------------------------------------------
ca_cider_coords %>%
  ggplot(mapping = aes(x = `Dim 1`, y = `Dim 2`)) + 
  geom_point(mapping = aes(color = type)) +
  ggrepel::geom_text_repel(mapping = aes(label = name),
            color = "darkblue") + 
  theme_void()


## ----using theme---------------------------------------------------------------------------------------------------------------------------
ca_cider_coords %>%
  ggplot(mapping = aes(x = `Dim 1`, y = `Dim 2`)) + 
  geom_point(mapping = aes(color = type)) +
  ggrepel::geom_text_repel(mapping = aes(label = name),
            color = "darkblue") + 
  theme_bw() + 
  # we use theme() to remove grid lines and move the legend, for example
  theme(panel.grid = element_blank(),
        legend.position = "bottom")


## ----element_text--------------------------------------------------------------------------------------------------------------------------
ca_cider_coords %>%
  ggplot(mapping = aes(x = `Dim 1`, y = `Dim 2`)) + 
  geom_point(mapping = aes(color = type)) +
  ggrepel::geom_text_repel(mapping = aes(label = name),
            color = "darkblue") +
  theme_bw() + 
  # let's make the axis titles bold and the labels serif-font
  theme(axis.title = element_text(face = "bold"),
        axis.text = element_text(family = "serif"))


## ----setting the default theme then making a ggplot----------------------------------------------------------------------------------------
theme_set(
  theme_bw() + 
    theme(panel.grid.minor = element_blank())
)

ca_cider_coords %>%
  ggplot(mapping = aes(x = `Dim 1`, y = `Dim 2`)) + 
  geom_point(mapping = aes(color = type)) +
  ggrepel::geom_text_repel(mapping = aes(label = name),
            color = "darkblue")


## ----ggplots are R objects-----------------------------------------------------------------------------------------------------------------
p <- 
  ca_cider_coords %>%
  ggplot(mapping = aes(x = `Dim 1`, y = `Dim 2`, color = type)) + 
  geom_point() +
  ggrepel::geom_text_repel(mapping = aes(label = name), 
                           # this command stops this layer from being added to
                           # the legend, to make it look cleaner
                           show.legend = FALSE)

p


## ----we can modify stored plots after the fact---------------------------------------------------------------------------------------------
p + scale_color_viridis_d()


## ----another example of posthoc plot modification------------------------------------------------------------------------------------------
# We'll pick 14 random colors from the colors R knows about

p + 
  scale_color_manual(values = c("wheat", "darkviolet"))


## ----cider contingency table---------------------------------------------------------------------------------------------------------------
cider_contingency


## ------------------------------------------------------------------------------------------------------------------------------------------
cider_contingency %>% 
  as_tibble(rownames = "sample") %>%
  pivot_longer(-sample) %>%
  ggplot(mapping = aes(x = name, y = value, fill = sample)) + 
  geom_col(position = "dodge") + 
  labs(x = NULL, y = NULL) + 
  # coord_*() functions affect how the axes are plotted;
  # coord_flip() switches the x- and y-axes
  coord_flip()


## ----cider-facets--------------------------------------------------------------------------------------------------------------------------
cider_contingency %>% 
  as_tibble(rownames = "sample") %>%
  pivot_longer(-sample) %>%
  ggplot(aes(x = sample, y = value)) + 
  geom_col(aes(fill = sample)) +
  scale_fill_manual(values = wesanderson::wes_palettes$AsteroidCity2) + 
  coord_flip() + 
  facet_wrap(~name, ncol = 6) + 
  labs(x = NULL, y = NULL, fill = NULL) + 
  theme(legend.position = "top")


## ----berry-penalty-plots-------------------------------------------------------------------------------------------------------------------
# First we wrangle our data using the tools we've learned to get penalties/lifts
# for each attributes
berry_penalty_analysis_data <- 
  berry_long_cata %>%
  group_by(berry, cata_variable, checked) %>%
  summarize(penalty_lift = mean(rating),
            count = n()) %>%
  ungroup() 

p1_berry_penalty <- 
  
  # We start by widening our data just a bit, and use a function to give some
  # better names to our mean values for when an attribute it checked (1) or not
  # (0).
  
  berry_penalty_analysis_data %>%
  select(-count) %>%
  pivot_wider(names_from = checked,
              values_from = penalty_lift,
              names_prefix = "checked_") %>%
  
  # Then we actually calculate the penalty/lift: what is the difference in the
  # mean liking when an attribute is checked or not?
  
  group_by(berry, cata_variable) %>%
  summarize(penalty_lift = checked_1 - checked_0) %>%
  
  # We have two kinds of CATA attibutes: visual assessment and by-mouth
  # assessment.  It would be nice to keep track.
  
  separate(cata_variable, 
           into = c("mode", "variable"), 
           sep = "_") %>%
  
  # Fix a typo
  
  mutate(mode = str_replace(mode, "appearane", "appearance")) %>%
  mutate(mode = case_when(mode == "taste" ~ "(T)",
                          mode == "appearance" ~ "(A)")) %>%
  unite(variable, mode, col = "cata_variable", sep = " ") %>%
  
  # We are using a function from tidytext that makes faceting the final figure
  # easier: reorder_within() makes a set of factors able to be ordered
  # differently within another variable.  In this case, we have different
  # attributes and different penalties within each berry by design
  
  mutate(cata_variable = tidytext::reorder_within(x = cata_variable,
                                                  by = penalty_lift,
                                                  within = berry)) %>%
  
  #And finally we plot!
  
  ggplot(mapping = aes(x = cata_variable, y = penalty_lift)) +
  geom_col(aes(fill = penalty_lift), color = "white", show.legend = FALSE) + 
  facet_wrap(~berry, scales = "free", nrow = 1) + 
  
  # To take advantage of our newly reordered factors, we also need to use the
  # matching tidytext::scale_x_reordered() function
  
  tidytext::scale_x_reordered() + 
  coord_flip() + 
  theme_classic() + 
  scale_fill_gradient(low = "tan", high = "darkgreen") + 
  labs(x = NULL, y = NULL,
       title = "Penalty / Lift Analysis",
       subtitle = "displays the mean difference (within berries) for when a CATA variable is checked\nor un-checked")

# Let's save it so we can come back to it later:
save(p1_berry_penalty, file = "data/goal-plot.RData")

p1_berry_penalty


## ----penalty analysis example, fig.width = 7-----------------------------------------------------------------------------------------------
p1_berry_penalty


## ----worse penalty analysis example, fig.width = 7-----------------------------------------------------------------------------------------
berry_penalty_analysis_data %>%
  select(-count) %>%
  pivot_wider(names_from = checked,
              values_from = penalty_lift,
              names_prefix = "checked_") %>%
  mutate(penalty_lift = checked_1 - checked_0) %>%
  ggplot(mapping = aes(x = cata_variable, y = penalty_lift)) +
  geom_col(aes(fill = penalty_lift), color = "white", show.legend = FALSE) + 
  facet_wrap(~berry, scales = "free", nrow = 2) +
  coord_flip() + 
  theme_classic() + 
  scale_fill_gradient(low = "tan", high = "darkgreen")


## ----ggsave--------------------------------------------------------------------------------------------------------------------------------
ggsave("img/penalty-lift-png.png", p1_berry_penalty)


## ----exporting a plot in multiple formats--------------------------------------------------------------------------------------------------
ggsave("img/penalty-lift-svg.svg", p1_berry_penalty)
ggsave("img/penalty-lift-jpeg.jpeg", p1_berry_penalty)


## ----viewing the plot inside R, fig.width = 7----------------------------------------------------------------------------------------------
p1_berry_penalty # If you're following along, this will look different in your R session!








## ----saving vector plots at different sizes and scales-------------------------------------------------------------------------------------
ggsave("img/penalty-lift-svg-7x4.svg", p1_berry_penalty,
       width = 7, height = 4, units = "in")

ggsave("img/penalty-lift-svg-14x8.svg", p1_berry_penalty,
       width = 14, height = 8, units = "in")

ggsave("img/penalty-lift-svg-14x8-rescale.svg", p1_berry_penalty,
       width = 14, height = 8, units = "in", scale = .5)








## ----capitalizing with stringr-------------------------------------------------------------------------------------------------------------
berry_penalty_analysis_data %>%
  select(berry) %>%
  mutate(Upper = str_to_upper(berry),
         Title = str_to_title(berry)) # Capitalizes the first letter of each word


## ----replacing underscores with str_replace------------------------------------------------------------------------------------------------
berry_penalty_analysis_data %>%
  select(-count) %>%
  pivot_wider(names_from = checked,
              values_from = penalty_lift,
              names_prefix = "checked_") %>%
  mutate(cata_variable = str_replace_all(cata_variable, "_", ": "))


## ----str_replace vs str_replace_all--------------------------------------------------------------------------------------------------------
str_replace("long_text_with_many_underscores", "_", " ")
str_replace_all("long_text_with_many_underscores", "_", " ")


## ----fixing typos with str_replace---------------------------------------------------------------------------------------------------------
berry_penalty_analysis_data %>%
  select(-count) %>%
  pivot_wider(names_from = checked,
              values_from = penalty_lift,
              names_prefix = "checked_") %>%
  mutate(cata_variable = str_replace_all(cata_variable,
                                         c("shapre" = "shape",
                                           "appearane" = "appearance",
                                           "_" = " ")))


## ----replacing parts of words--------------------------------------------------------------------------------------------------------------
#This can lead to unintentional side-effects
c("nocolor", "none", "cornonthecob", "anode") %>%
  str_replace_all("no", " NO ")

#Or it can be useful for fixing lots of similar problems all at once
berry_penalty_analysis_data %>%
  select(-count) %>%
  pivot_wider(names_from = checked,
              values_from = penalty_lift,
              names_prefix = "checked_") %>%
  mutate(cata_variable = str_replace_all(cata_variable,
                                         c("not" = "not ",
                                           "good" = "good ",
                                           "uneven" = "uneven ",
                                           "_" = " ")))


## ----str_replace with periods--------------------------------------------------------------------------------------------------------------
str_replace_all("long.text.with.many.periods", ".", " ") # Replaces everything
str_replace_all("long.text.with.many.periods", "\\.", " ") # Replaces only dots


## ----removing the legend and the axis ticks------------------------------------------------------------------------------------------------
berry_long_liking %>%
  ggplot(aes(x = Scale, y = Liking, color = Scale)) +
  ggbeeswarm::geom_quasirandom() +
  facet_wrap(~ Attribute) +
  theme_bw() +
  theme(panel.grid = element_blank(),
        axis.ticks = element_blank(),
        legend.position = "none")


## ----excluding geoms from the legend-------------------------------------------------------------------------------------------------------
ca_cider$col$coord %>%
  as_tibble(rownames = "Attribute") %>%
  mutate(Modality = case_when(Attribute == "Sweet" ~ "Taste",
                              Attribute == "Bitter" ~ "Taste",
                              Attribute == "Sour" ~ "Taste",
                              Attribute == "Smooth" ~ "Mouthfeel",
                              Attribute == "Dry" ~ "Mouthfeel",
                              Attribute == "FullBodied" ~ "Mouthfeel",
                              .default = "Aroma")) %>%
  ggplot(aes(x = `Dim 1`, y = `Dim 2`,
             label = Attribute, color = Modality)) -> ca_cider_colored

ca_cider_colored +
  geom_point() +
  ggrepel::geom_text_repel()

ca_cider_colored +
  geom_point() +
  ggrepel::geom_text_repel(show.legend = FALSE)


## ----default categorical axis in ggplot2---------------------------------------------------------------------------------------------------
long_cider_data %>%
  filter(checked == 1) %>%
  ggplot(aes(x = cata_variable)) +
  geom_bar() +
  coord_flip() +
  facet_grid(vars(Temperature), vars(Sample_Name))


## ----manually making ordered factors-------------------------------------------------------------------------------------------------------
long_cider_data %>%
  mutate(cata_variable = factor(cata_variable,
                                levels = c("Sweet", "Sour", "Bitter",
                                           "Smooth", "Dry", "FullBodied",
                                           "Light",
                                           "Fruity", "Berries", "Fresh_Apples",
                                           "Floral", "Spice",
                                           "Herbal", "Woody", "Earthy",
                                           "Funky", "Fermented", "Vomit",
                                           "Synthetic", "Candy",
                                           "Metallic", "Alcohol"))) -> long_cider_manual_factors

long_cider_manual_factors %>%
  filter(checked == 1) %>%
  ggplot(aes(x = cata_variable)) +
  geom_bar() +
  coord_flip() +
  facet_grid(vars(Temperature), vars(Sample_Name))


## ----showing the order of the factor-------------------------------------------------------------------------------------------------------
long_cider_manual_factors %>%
  distinct(cata_variable) %>%
  mutate(variable_number = as.numeric(cata_variable))


## ----using another variable to order a factor----------------------------------------------------------------------------------------------
long_cider_data %>%
  # Counting the number of times each attribute is used across all products:
  group_by(cata_variable) %>%
  mutate(variable_count = sum(checked)) %>%
  ungroup() %>%
  # Arranging from least-to-most used:
  arrange(variable_count) %>%
  # Converting to a factor, so the least-used will be 1st, then the next:
  mutate(cata_variable = factor(cata_variable, levels = unique(cata_variable),
                            ordered = TRUE),
         variable_number = as.numeric(cata_variable)) -> long_cider_frequency_factors

#Now the plot:
long_cider_frequency_factors %>%
  filter(checked == 1) %>%
  ggplot(aes(x = cata_variable)) +
  geom_bar() +
  coord_flip() +
  facet_grid(vars(Temperature), vars(Sample_Name))


## ----tidytext reordering within facets-----------------------------------------------------------------------------------------------------
long_cider_data %>%
  # Counting the number of times each attribute is used across all products:
  group_by(Sample_Name, Temperature, cata_variable) %>%
  mutate(Product = str_c(Sample_Name, " (", Temperature, ")"),
         variable_count = sum(checked),
         cata_variable = tidytext::reorder_within(cata_variable,
                                                  by = variable_count,
                                                  within = list(Sample_Name, Temperature))) %>%
  ungroup() %>%
  filter(checked == 1) %>%
  ggplot(aes(x = cata_variable)) +
  geom_bar() +
  tidytext::scale_x_reordered() +
  coord_flip() +
  # This will not work with facet_grid, because it forces all plots in a row to
  # share a vertical axis, even with scales = "free"
  facet_wrap(~ Product,
             scales = "free")


## ----final walkthrough of penalty analysis, fig.width = 7----------------------------------------------------------------------------------
berry_penalty_analysis_data %>%
  select(-count) %>%
  pivot_wider(names_from = checked,
              values_from = penalty_lift,
              names_prefix = "checked_") %>%
  separate(cata_variable, 
           into = c("mode", "variable"), 
           sep = "_") %>%
  mutate(penalty_lift = checked_1 - checked_0,
         mode = case_when(mode == "taste" ~ "(T)",
                          mode == "appearance" ~ "(A)",
                          mode == "appearane" ~ "(A)")) %>%
  unite(variable, mode, col = "cata_variable", sep = " ") %>%
  mutate(cata_variable = tidytext::reorder_within(x = cata_variable,
                                                  by = penalty_lift,
                                                  within = berry)) %>%
  ggplot(mapping = aes(x = cata_variable, y = penalty_lift)) +
  geom_col(aes(fill = penalty_lift), color = "white", show.legend = FALSE) + 
  facet_wrap(~berry, scales = "free", nrow = 1) + 
  tidytext::scale_x_reordered() + 
  coord_flip() + 
  theme_classic() + 
  scale_fill_gradient(low = "tan", high = "darkgreen") + 
  labs(x = NULL, y = NULL,
       title = "Penalty / Lift Analysis",
       subtitle = "displays the mean difference (within berries) for when a CATA variable is checked\nor un-checked")


## ----multivariate analyses using FactoMineR------------------------------------------------------------------------------------------------
load("data/cleaned-data.RData")

cider_contingency %>%
  FactoMineR::CA(graph = FALSE) -> ca_cider

berry_mfa_summary %>%
  FactoMineR::MFA(group = c(sum(str_detect(colnames(berry_mfa_summary), "^cata_")),
                            sum(str_detect(colnames(berry_mfa_summary), "^liking_"))),
                  type = c("f","s"), graph = FALSE,
                  name.group = c("CATA","Liking")) -> berry_mfa_res

save(ca_cider,
     berry_mfa_res,
     file = "data/svd-results.RData")


## ----using geom_text with 22 sensory attributes--------------------------------------------------------------------------------------------
#Let's use the cider CA example from before.
#We can make our own plots from the coordinates.
ca_cider$col$coord %>%
  as_tibble(rownames = "Attribute") %>%
  ggplot(aes(x = `Dim 1`, y = `Dim 2`, label = Attribute)) +
  theme_bw() -> ca_cider_termplot

ca_cider_termplot +
  geom_text()


## ----using geom_label with 22 sensory attributes-------------------------------------------------------------------------------------------
ca_cider_termplot +
  geom_label()


## ----using geom_label_repel with 22 sensory attributes-------------------------------------------------------------------------------------
ca_cider_termplot +
  ggrepel::geom_label_repel()


## ----geom_text_repel with a set seed will look the same every time-------------------------------------------------------------------------
ca_cider_termplot +
  ggrepel::geom_label_repel(seed = 12345)


## ----geom_text_repel text borders, message = FALSE-----------------------------------------------------------------------------------------
raw_cider_data %>%
  mutate(Product = str_c(Sample_Name, Temperature, sep = " ")) %>%
  group_by(Product) %>%
  summarize(Liking = mean(Liking)) %>%
  left_join(ca_cider$row$coord %>%
              as_tibble(rownames = "Product")) %>%
  rename_with(~ str_replace_all(.x, " ", ".")) -> ca_cider_productcoord

#This is NOT a statistically sound preference model, this is just for demonstration
ca_cider_prefmod <- lm(Liking ~ Dim.1 * Dim.2, data = ca_cider_productcoord)
expand.grid(Dim.1 = seq(min(ca_cider$col$coord[, "Dim 1"]) - 0.1,
                    max(ca_cider$col$coord[, "Dim 1"]) + 0.1,
                    by = 0.01),
            Dim.2 = seq(min(ca_cider$col$coord[, "Dim 2"]) - 0.1,
                    max(ca_cider$col$coord[, "Dim 2"]) + 0.1,
                    by = 0.01)) %>%
  mutate(., Liking = predict(ca_cider_prefmod, newdata = .)) -> ca_cider_prefinterp

ca_cider_termplot +
  geom_contour_filled(aes(x = Dim.1, y = Dim.2, z = Liking, fill = after_stat(level)),
                  inherit.aes = FALSE,
                  data = ca_cider_prefinterp) + 
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  ggrepel::geom_text_repel(size = 6, color = "white", bg.color = "grey7")


## ----using ggbeeswarm to compare ordinal distributions-------------------------------------------------------------------------------------
#The jitter plot is actually not very helpful with this many points
berry_long_liking %>%
  ggplot(aes(x = Scale, y = Liking, color = Scale)) +
  geom_jitter() +
  facet_wrap(~ Attribute) +
  theme_bw()

#geom_beeswarm() will also have the same problem, but geom_quasirandom()
#visualizes the density at each "bin" without us having to specify bins.
#So these are easy to compare
berry_long_liking %>%
  ggplot(aes(x = Scale, y = Liking, color = Scale)) +
  ggbeeswarm::geom_quasirandom() +
  facet_wrap(~ Attribute) +
  theme_bw()


## ----adding a unit circle with ggforce-----------------------------------------------------------------------------------------------------
berry_mfa_res$quanti.var$coord %>%
  as_tibble(rownames = "Modality") %>%
  ggplot() +
  geom_segment(aes(x = 0, y = 0, xend = Dim.1, yend = Dim.2), arrow = arrow()) +
  ggforce::geom_circle(aes(x0 = 0, y0 = 0, r = 1), color = "blue") +
  ggrepel::geom_text_repel(aes(x = Dim.1, y = Dim.2, label = Modality)) +
  theme_bw() +
  theme(aspect.ratio = 1)


## ----the minimal themes in cowplot---------------------------------------------------------------------------------------------------------
berry_long_liking %>%
  ggplot(aes(x = Scale, y = Liking, color = Scale)) +
  ggbeeswarm::geom_quasirandom() +
  facet_wrap(~ Attribute) +
  cowplot::theme_minimal_hgrid()


## ----FactoMineR makes ggplots--------------------------------------------------------------------------------------------------------------
#FactoMineR uses ggplot for its internal plotting,
#Which is why we can assign the output to a variable
#and not see the plot right away
#(although the CA() function will also display several plots by default)
cider_contingency %>%
  FactoMineR::CA(graph = FALSE) %>%
  FactoMineR::plot.CA() -> ca_cider_biplot_facto

#The ca package, meanwhile, uses base plotting.
#You can tell because it prints this plot immediately.
cider_contingency %>%
  ca::ca() %>%
  ca::plot.ca() -> ca_cider_biplot_green


## ----using class to find ggplots-----------------------------------------------------------------------------------------------------------
class(ca_cider_termplot) # Made with ggplot() ourselves
class(ca_cider_biplot_facto) # Made with ggplot-based FactoMineR
class(ca_cider_biplot_green) # Made with ca::ca(), not a plot at all
ca_cider_biplot_green # It's two tables of coordinates!


## ----you can see saved ggplots by calling the variable-------------------------------------------------------------------------------------
ca_cider_biplot_facto


## ----you can change many things about FactoMineR plots with ggplot semantics---------------------------------------------------------------
ca_cider_biplot_facto +
  theme(panel.grid = element_blank(), # Removes the axis lines
        plot.title = element_blank()) + # Removes the title
  xlim(-1,1) + # Extends the x limits, with a warning
  scale_color_brewer(type = "qual", palette = "Dark2")
  # Silently fails to change the color scale


## ----you can easily add new geoms to FactoMineR plots--------------------------------------------------------------------------------------
liking_arrow <- data.frame(x1 = 0, y1 = 0, x2 = -0.4, y2 = -0.1, text = "Liking")

ca_cider_biplot_facto +
  geom_segment(aes(x= x1, y = y1, xend = x2, yend = y2), color = "orange",
               arrow = arrow(length = unit(0.03, "npc")), linewidth = 1,
               data = liking_arrow) +
  geom_text(aes(x = x2, y = y2, label = text), color = "orange",
            hjust = "outward", vjust = "outward", fontface = "italic",
            data = liking_arrow)


## ----remember faceting, message = FALSE----------------------------------------------------------------------------------------------------
raw_cider_data %>%
  pivot_longer(Fresh_Apples:Synthetic) %>%
  group_by(Sample_Name, Temperature, name) %>%
  summarize(total = sum(value)) %>%
  ggplot(aes(x = interaction(Sample_Name, Temperature), y = total)) + 
  geom_col(aes(fill = Sample_Name)) +
  scale_fill_manual(values = wesanderson::wes_palettes$FantasticFox1) + 
  coord_flip() + 
  labs(x = NULL, y = NULL, fill = NULL) +
  theme_bw() + 
  theme(legend.position = "top",
        panel.grid = element_blank()) -> cider_count_plot

cider_count_plot +
  facet_wrap(~name, ncol = 6)


## ----faceted plots with different x-axes---------------------------------------------------------------------------------------------------
cider_count_plot +
  facet_wrap(~name, ncol = 6,
             scales = "free_x") # Each plot now has a different x-axis


## ----combining plots with patchwork, fig.width=7, warning = FALSE--------------------------------------------------------------------------
library(patchwork)
plot(berry_mfa_res, choix = "var") + plot(berry_mfa_res, partial = "all")


## ----arranging plots vertically with patchwork, fig.height=7, warning = FALSE--------------------------------------------------------------
plot(berry_mfa_res, choix = "var") / plot(berry_mfa_res, partial = "all")


## ----collecting legends at the bottom of a patchwork ensemble, fig.width = 7, warning = FALSE----------------------------------------------
plot(berry_mfa_res, choix = "var") + plot(berry_mfa_res, partial = "all") +
  plot_layout(guides = "collect") &
  theme(plot.title = element_blank(),
        legend.position = "bottom")


## ----more complex patchwork layout, fig.width = 7, warning = FALSE-------------------------------------------------------------------------
plot(berry_mfa_res, partial = "all") +
  (plot(berry_mfa_res, choix = "var") +
  plot(berry_mfa_res, choix = "freq", invisible = "ind")) +
  plot_layout(guides = "collect", ncol = 1, widths = 2) &
  theme(plot.title = element_blank(),
        axis.title = element_blank(),
        legend.position = "bottom")


## ----plotting a list, fig.width = 7, warning = FALSE---------------------------------------------------------------------------------------
berry_mfa_res$separate.analyses %>%
  lapply(function(x) {
    x$ind$coord %>%
      as_tibble(rownames = "Berry") %>%
      ggplot(aes(x = Dim.1, y = Dim.2)) +
      geom_point()
  }) %>%
  cowplot::plot_grid(plotlist = ., labels = names(.))

#You can also pipe your list into patchwork::wrap_plots()
#if you have the latest version of patchwork.
#It's a fairly new package, so it gains big new features very often.


## ----labeling plots, fig.width=7, warning = FALSE------------------------------------------------------------------------------------------
plot(berry_mfa_res, choix = "var") + plot(berry_mfa_res, partial = "all") +
  plot_layout(guides = "collect") +
  plot_annotation(tag_levels = 'A') &
  theme(plot.title = element_blank(),
        legend.position = "bottom")

cowplot::plot_grid(plot(berry_mfa_res, choix = "var"),
                   plot(berry_mfa_res, partial = "all"),
                   labels = "AUTO")
#Cowplot doesn't have a way to combine or move legends.
#You'd have to move the legends *before* using plot_grid()


## ----moving plot labels, fig.width = 7, warning = FALSE------------------------------------------------------------------------------------
plot(berry_mfa_res, choix = "var") + theme(plot.tag.position = c(0.2, 0.95)) +
  plot(berry_mfa_res, partial = "all") + theme(plot.tag.position = c(0.12, 0.95)) +
  plot_layout(guides = "collect") +
  plot_annotation(tag_levels = 'A') &
  theme(plot.title = element_blank(),
        legend.position = "bottom")

cowplot::plot_grid(plot(berry_mfa_res, choix = "var"),
                   plot(berry_mfa_res, partial = "all"),
                   labels = "AUTO",
                   label_y = 0.8)




