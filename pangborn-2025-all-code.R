## ----setup------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
library(tidyverse)


## ----workshop-code, child = c("01-import-motivation-export.Rmd", "02-ggplot2-basics.Rmd", "03-finetuning-ggplot.Rmd", "04-conclusions.Rmd")---------------------------------------------------

## ----libraries-2------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# This package is actually a set of utility packages we will use a lot
library(tidyverse)


## ----get-working-directory--------------------------------------------------------------------------------------------------------------------------------------------------------------------
getwd()


## ----store-the-data, message=FALSE------------------------------------------------------------------------------------------------------------------------------------------------------------
raw_berry_data <- read_csv(file = "data/clt-berry-data.csv")
raw_cider_data <- read_csv(file = "data/CiderDryness_SensoryDATA.csv")


## ----cider CA final, message = FALSE, fig.align='center', fig.width = 7, results = FALSE------------------------------------------------------------------------------------------------------
raw_cider_data <- 
  read_csv("data/CiderDryness_SensoryDATA.csv")

cider_samples <-
  raw_cider_data %>%
  select(Sample_Name, Temperature) %>%
  unite(Sample_Name, Temperature, col = "sample", sep = " ", remove = FALSE) %>%
  distinct()

cider_contingency <- 
  raw_cider_data %>%
  select(Sample_Name, Temperature, Fresh_Apples:Synthetic) %>%
  unite(Sample_Name, Temperature, col = "sample", sep = " ") %>%
  group_by(sample) %>%
  summarize(across(where(is.numeric), ~sum(.))) %>%
  column_to_rownames("sample")
  
ca_cider <- 
  cider_contingency %>%
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
         name = str_replace(name, "FullBodied", "Full Bodied"),
         modality = if_else(type == "col",
                            case_when(name == "Sweet" ~ "Taste",
                              name == "Bitter" ~ "Taste",
                              name == "Sour" ~ "Taste",
                              name == "Smooth" ~ "Mouthfeel",
                              name == "Dry" ~ "Mouthfeel",
                              name == "FullBodied" ~ "Mouthfeel",
                              .default = "Aroma"),
                            NA))

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


## ----berry-full-penalty, message=FALSE, fig.align='center', fig.width=7, fig.height=4, results=FALSE------------------------------------------------------------------------------------------
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
  summarize(rating = mean(rating),
            count = n()) %>%
  pivot_wider(names_from = checked,
              values_from = c(rating, count),
              names_prefix = "checked_") %>%
  mutate(penalty_lift = rating_checked_1 - rating_checked_0,
         count = count_checked_1, .keep = "none") %>%
  ungroup() %>%
  # We can tidy up our CATA labels
  separate(cata_variable, 
           into = c("mode", "variable"), 
           sep = "_",
           remove = FALSE) %>%
  # Fix a typo
  mutate(mode = str_replace(mode, "appearane", "appearance")) %>%
  mutate(mode = case_when(mode == "taste" ~ "(T)",
                          mode == "appearance" ~ "(A)")) %>%
  unite(variable, mode, col = "cata_variable_clean", sep = " ")

p1_berry_penalty <- 
  
  berry_penalty_analysis_data %>%
  # We are using a function from tidytext that makes faceting the final figure
  # easier
  mutate(cata_variable_clean = tidytext::reorder_within(x = cata_variable_clean,
                                                        by = penalty_lift,
                                                        within = berry)) %>%
  #And finally we plot!
  ggplot(mapping = aes(x = cata_variable_clean, y = penalty_lift)) +
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


## ----CA biplot made by FactoMineR-------------------------------------------------------------------------------------------------------------------------------------------------------------
p3_cider_factominer <- plot(ca_cider)
p3_cider_factominer


## ----editing the FactoMineR plot--------------------------------------------------------------------------------------------------------------------------------------------------------------
p3_cider_factominer +
  theme_dark() + 
  labs(caption = "Now we can say some more things!", 
       subtitle = "of 6 ciders tasted by 48 subjects")


## ----limits of automatic plots----------------------------------------------------------------------------------------------------------------------------------------------------------------
p3_cider_factominer + 
  scale_color_manual(values = c("darkorange", "darkgreen"))


## ----demonstrating write_csv------------------------------------------------------------------------------------------------------------------------------------------------------------------
# We will keep in the tidyverse idiom with readr::write_csv()
write_csv(x = berry_penalty_analysis_data,
          file = "data/berry-penalty-data.csv")


## ----demonstrating write_rds------------------------------------------------------------------------------------------------------------------------------------------------------------------
berry_penalty_analysis_data %>%
  write_rds(file = "data/berry-penalty-data.rds")


## ----saving multiple data objects together----------------------------------------------------------------------------------------------------------------------------------------------------
save(berry_penalty_analysis_data,
     ca_cider_coords,
     ca_cider,
     cider_contingency,
     file = "data/workshop-data.RData")

rm(berry_penalty_analysis_data,
     ca_cider_coords,
     ca_cider,
     cider_contingency)

load(file = "data/workshop-data.RData")


## ----non-working schematic of a ggplot, eval = FALSE------------------------------------------------------------------------------------------------------------------------------------------
## # The ggplot() function creates your plotting environment.  We usually save it to a variable in R so that we can use the plug-n-play functionality of ggplot without retyping a bunch of nonsense
## p <- ggplot(mapping = aes(x = <a variable>, y = <another variable>, ...),
##             data = <your data>)
## 
## # Then, you can add various ways of plotting data to make different visualizations.
## p +
##   geom_<your chosen way of plotting>(...) +
##   theme_<your chosen theme> +
##   ...


## ----a first ggplot---------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# We start with our data and pipe it into ggplot
ca_cider_coords %>%
  # Here we set up the base plot
  ggplot(mapping = aes(x = `Dim 1`, y = `Dim 2`)) + 
   # Here we tell our base plot to add points
  geom_point()                          


## ----what we are plotting in this example-----------------------------------------------------------------------------------------------------------------------------------------------------
ca_cider_coords %>% 
  select(`Dim 1`, `Dim 2`)


## ----switching geom switches the way the data map---------------------------------------------------------------------------------------------------------------------------------------------
ca_cider_coords %>%
  ggplot(mapping = aes(x = `Dim 1`, y = `Dim 2`)) + 
  # connecting our points with a line makes even less sense
  geom_line()


## ----geoms are layers in a plot---------------------------------------------------------------------------------------------------------------------------------------------------------------
ca_cider_coords %>%
  ggplot(mapping = aes(x = `Dim 1`, y = `Dim 2`)) + 
  geom_point() +
  # add a layer with the names of the points
  geom_text(mapping = aes(label = name))


## ----here are some other parts of the plot we can control with data---------------------------------------------------------------------------------------------------------------------------
ca_cider_coords %>%
  ggplot(mapping = aes(x = `Dim 1`, y = `Dim 2`, color = type)) + 
  geom_point() +
  geom_text(mapping = aes(label = name))


## ----color only one layer---------------------------------------------------------------------------------------------------------------------------------------------------------------------
ca_cider_coords %>%
  ggplot(mapping = aes(x = `Dim 1`, y = `Dim 2`)) + 
  # we will only color the points, not the text
  geom_point(mapping = aes(color = type)) +
  geom_text(mapping = aes(label = name))


## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
ca_cider_coords %>%
  ggplot(mapping = aes(x = `Dim 1`, y = `Dim 2`)) + 
  geom_point(mapping = aes(color = type)) +
  geom_text(mapping = aes(label = name),
            hjust = "outward",
            color = "darkblue")


## ----showing ggrepel--------------------------------------------------------------------------------------------------------------------------------------------------------------------------
ca_cider_coords %>%
  ggplot(mapping = aes(x = `Dim 1`, y = `Dim 2`)) + 
  geom_point(mapping = aes(color = type)) +
  # here we opt for not loading the package
  ggrepel::geom_text_repel(mapping = aes(label = name),
            color = "darkblue") +
  scale_color_manual(values = c("darkorange", "darkgreen"))


## ----adding a different theme-----------------------------------------------------------------------------------------------------------------------------------------------------------------
ca_cider_coords %>%
  ggplot(mapping = aes(x = `Dim 1`, y = `Dim 2`)) + 
  geom_point(mapping = aes(color = type)) +
  ggrepel::geom_text_repel(mapping = aes(label = name),
            color = "darkblue") +
  # here we switch to a black and white theme
  theme_bw()


## ----using the theme functions----------------------------------------------------------------------------------------------------------------------------------------------------------------
ca_cider_coords %>%
  ggplot(mapping = aes(x = `Dim 1`, y = `Dim 2`)) + 
  geom_point(mapping = aes(color = type)) +
  ggrepel::geom_text_repel(mapping = aes(label = name),
            color = "darkblue") + 
  theme_void()


## ----using theme------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
ca_cider_coords %>%
  ggplot(mapping = aes(x = `Dim 1`, y = `Dim 2`)) + 
  geom_point(mapping = aes(color = type)) +
  ggrepel::geom_text_repel(mapping = aes(label = name),
            color = "darkblue") + 
  theme_bw() + 
  # we use theme() to remove grid lines and move the legend, for example
  theme(panel.grid = element_blank(),
        legend.position = "bottom")


## ----element_text-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------
ca_cider_coords %>%
  ggplot(mapping = aes(x = `Dim 1`, y = `Dim 2`)) + 
  geom_point(mapping = aes(color = type)) +
  ggrepel::geom_text_repel(mapping = aes(label = name),
            color = "darkblue") +
  theme_bw() + 
  # let's make the axis titles bold and the labels serif-font
  theme(axis.title = element_text(face = "bold"),
        axis.text = element_text(family = "serif"))


## ----setting the default theme then making a ggplot-------------------------------------------------------------------------------------------------------------------------------------------
theme_set(
  theme_bw() + 
    theme(panel.grid.minor = element_blank())
)

ca_cider_coords %>%
  ggplot(mapping = aes(x = `Dim 1`, y = `Dim 2`)) + 
  geom_point(mapping = aes(color = type)) +
  ggrepel::geom_text_repel(mapping = aes(label = name),
            color = "darkblue")


## ----ggplots are R objects--------------------------------------------------------------------------------------------------------------------------------------------------------------------
p <- 
  ca_cider_coords %>%
  ggplot(mapping = aes(x = `Dim 1`, y = `Dim 2`, color = type)) + 
  geom_point() +
  ggrepel::geom_text_repel(mapping = aes(label = name), 
                           # this command stops this layer from being added to
                           # the legend, to make it look cleaner
                           show.legend = FALSE)

p


## ----we can modify stored plots after the fact------------------------------------------------------------------------------------------------------------------------------------------------
p + scale_color_viridis_d()


## ----another example of posthoc plot modification---------------------------------------------------------------------------------------------------------------------------------------------
# We'll pick 14 random colors from the colors R knows about

p + 
  scale_color_manual(values = c("wheat", "darkviolet"))


## ----cider contingency table------------------------------------------------------------------------------------------------------------------------------------------------------------------
cider_contingency


## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
cider_contingency %>% 
  as_tibble(rownames = "sample") %>%
  pivot_longer(-sample) %>%
  ggplot(mapping = aes(x = name, y = value, fill = sample)) + 
  geom_col(position = "dodge") + 
  labs(x = NULL, y = NULL) + 
  # coord_*() functions affect how the axes are plotted;
  # coord_flip() switches the x- and y-axes
  coord_flip()


## ----cider-facets-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------
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


## ----berry-final-plot-------------------------------------------------------------------------------------------------------------------------------------------------------------------------
p1_berry_penalty <- 
  
  berry_penalty_analysis_data %>%
  
  # We are using a function from tidytext that makes faceting the final figure
  # easier: reorder_within() makes a set of factors able to be ordered
  # differently within another variable.  In this case, we have different
  # attributes and different penalties within each berry by design
  
  mutate(cata_variable_clean = tidytext::reorder_within(x = cata_variable_clean,
                                                        by = penalty_lift,
                                                        within = berry)) %>%
  
  #And finally we plot!
  
  ggplot(mapping = aes(x = cata_variable_clean, y = penalty_lift)) +
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
       subtitle = "displays the mean difference (within berries) for when a CATA variable is checked")

p1_berry_penalty


## ----cider-final-plot-------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# First, we have some string-wrangling to do so that our various labels are
# more descriptive and seem like a person wrote them, rather than a computer.

nice_cider_labels <-
  labs(x = str_c("Dimension 1, ", round(ca_cider$eig[1, 2], 1), "% of inertia"),
       y = str_c("Dimension 2, ", round(ca_cider$eig[2, 2], 1), "% of inertia"),
       subtitle = "Correspondence Analysis biplot (symmetric)",
       title = "Effect of cider serving temperature on consumer sensory perception")

p2_ca_cider_cata <- 
  ca_cider_coords %>%
  
  # A few specific aesthetics don't have scale_*() functions and require that
  # you make a column with the exact names of the "value". Fontface is one of them.
  
  mutate(font = if_else(type == "row", "plain", "italic")) %>%
  
  # And now we plot!
  
  ggplot(aes(x = `Dim 1`, y = `Dim 2`)) +
  
  # We can use geoms to make darker lines at the origin of each axis.
  # This goes first so that all of our data points are drawn on top.
  
  geom_vline(xintercept = 0, linetype = "dashed", color = "darkgrey") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "darkgrey") +
  
  # We want to represent our samples with geom_point and our attributes
  # with geom_text, so we need to do some creative filtering.
  
  geom_point(aes(color = type, shape = Temperature),
             data = ca_cider_coords %>% filter(type == "row"),
             size = 3) +
  ggrepel::geom_text_repel(aes(label = name, color = type, fontface = font),
                           show.legend = FALSE) +
  
  # and now we do our fine-tuning.
  
  coord_equal() + 
  theme_linedraw() +
  theme(legend.position = "bottom") +
  nice_cider_labels + 
  scale_color_manual(values = c("darkorange", "darkgreen")) +
  scale_shape_manual(values = c(8, 16)) +
  guides(shape = guide_legend(),
         color = "none")

p2_ca_cider_cata



## ----saving-final-plots-----------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Let's save them so we can come back to them later:
save(p1_berry_penalty,
     p2_ca_cider_cata,
     file = "data/goal-plots.RData")


## ----pretty plot examples, fig.width = 7------------------------------------------------------------------------------------------------------------------------------------------------------
p1_berry_penalty
p2_ca_cider_cata


## ----ggsave-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
ggsave("img/penalty-lift.jpeg", p1_berry_penalty)




## ----ggsave-sized-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------
ggsave("img/penalty-lift-10x6.jpeg", p1_berry_penalty,
       width = 10,
       height = 6,
       units = "in") #inches




## ----fqap-resolution--------------------------------------------------------------------------------------------------------------------------------------------------------------------------
ggsave("img/penalty-lift-double-column-width.jpeg", p1_berry_penalty,
       width = 10,
       height = 6,
       units = "in", #inches
       dpi = 400) #slightly above 3750 / 10. Always err on the side of slightly too big!!




## ----worse berry penalty analysis example, fig.width = 7--------------------------------------------------------------------------------------------------------------------------------------
berry_penalty_analysis_data %>%
  ggplot(mapping = aes(x = cata_variable, y = penalty_lift)) +
  geom_col(aes(fill = penalty_lift), color = "white", show.legend = FALSE) + 
  facet_wrap(~berry) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 90))


## ----coord_flip-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
berry_penalty_analysis_data %>%
  ggplot(mapping = aes(x = cata_variable, y = penalty_lift)) +
  geom_col(aes(fill = penalty_lift), color = "white", show.legend = FALSE) + 
  facet_wrap(~berry, nrow = 1) +
  theme_classic() +
  coord_flip()


## ----free scales on ordered data--------------------------------------------------------------------------------------------------------------------------------------------------------------
berry_penalty_analysis_data %>%
  ggplot(mapping = aes(x = cata_variable_clean, y = penalty_lift)) +
  geom_col(aes(fill = penalty_lift), color = "white", show.legend = FALSE) + 
  facet_wrap(~berry, nrow = 1, scales = "free") +
  coord_flip() + 
  theme_classic()


## ----default categorical axis in ggplot2------------------------------------------------------------------------------------------------------------------------------------------------------
berry_penalty_analysis_data %>%
  ggplot(mapping = aes(x = cata_variable, y = penalty_lift)) +
  geom_col(aes(fill = penalty_lift), color = "white", show.legend = FALSE) + 
  facet_wrap(~berry, nrow = 1) +
  coord_flip() + 
  theme_classic()


## ----tidytext reordering within facets--------------------------------------------------------------------------------------------------------------------------------------------------------
berry_penalty_analysis_data %>%
  mutate(cata_variable_clean = tidytext::reorder_within(cata_variable_clean,
                                                  by = count,
                                                  within = berry)) %>%
  ggplot(mapping = aes(x = cata_variable_clean, y = penalty_lift)) +
  geom_col(aes(fill = penalty_lift), color = "white", show.legend = FALSE) + 
  tidytext::scale_x_reordered() +
  facet_wrap(~berry, scales = "free", nrow = 1) +
  coord_flip() + 
  theme_classic()


## ----capitalizing with stringr----------------------------------------------------------------------------------------------------------------------------------------------------------------
berry_penalty_analysis_data %>%
  select(berry) %>%
  mutate(Upper = str_to_upper(berry),
         Title = str_to_title(berry)) # Capitalizes the first letter of each word


## ----replacing underscores with str_replace---------------------------------------------------------------------------------------------------------------------------------------------------
berry_penalty_analysis_data %>%
  mutate(cata_variable = str_replace_all(cata_variable, "_", ": "))


## ----str_replace vs str_replace_all-----------------------------------------------------------------------------------------------------------------------------------------------------------
str_replace("long_text_with_many_underscores", "_", " ")
str_replace_all("long_text_with_many_underscores", "_", " ")


## ----fixing typos with str_replace------------------------------------------------------------------------------------------------------------------------------------------------------------
berry_penalty_analysis_data %>%
  mutate(cata_variable = str_replace_all(cata_variable,
                                         c("shapre" = "shape",
                                           "appearane" = "appearance",
                                           "_" = " ")))


## ----replacing parts of words-----------------------------------------------------------------------------------------------------------------------------------------------------------------
#This can lead to unintentional side-effects
c("nocolor", "none", "cornonthecob", "anode") %>%
  str_replace_all("no", " NO ")

#Or it can be useful for fixing lots of similar problems all at once
berry_penalty_analysis_data %>%
  mutate(cata_variable = str_replace_all(cata_variable,
                                         c("not" = "not ",
                                           "good" = "good ",
                                           "uneven" = "uneven ",
                                           "_" = " ")))


## ----str_replace with periods-----------------------------------------------------------------------------------------------------------------------------------------------------------------
str_replace_all("long.text.with.many.periods", ".", " ") # Replaces everything
str_replace_all("long.text.with.many.periods", "\\.", " ") # Replaces only dots


## ----using separate to split one text column into multiple------------------------------------------------------------------------------------------------------------------------------------
berry_penalty_by_modality <- berry_penalty_analysis_data %>%
  separate_wider_delim(cata_variable, "_",
                       names = c("mode", "variable"),
                       cols_remove = FALSE)

berry_penalty_by_modality

# Which would let you easily color-code your ggplot based on the new mode column

berry_penalty_by_modality %>%
  ggplot(aes(x = cata_variable, y = count, fill = mode)) +
  geom_col() +
  coord_flip()


## ----combining text columns-------------------------------------------------------------------------------------------------------------------------------------------------------------------
berry_penalty_by_modality %>%
  mutate(cata_variable_2 = str_c(variable, mode, sep = " ")) %>%
  unite(variable, mode, col = "cata_variable_3", sep = " ")


## ----using multiple string manipulations in practice------------------------------------------------------------------------------------------------------------------------------------------
berry_penalty_analysis_data %>%
  # Fixing typos
  mutate(cata_variable = str_replace_all(cata_variable, c("appearane" = "appearance",
                                                      "shapre" = "shape"))) %>%
  # Separating the modality from the attribute name
  separate_wider_delim(cata_variable, "_",
                       names = c("mode", "variable"),
                       cols_remove = FALSE) %>%
  # Shorthand for the taste and appearance variables
  mutate(mode = case_when(mode == "taste" ~ "(T)",
                          mode == "appearance" ~ "(A)")) %>%
  # Putting them back together
  unite(variable, mode, col = "cata_variable_clean", sep = " ")


## ----redundant legend-------------------------------------------------------------------------------------------------------------------------------------------------------------------------
berry_penalty_analysis_data %>%
  mutate(cata_variable_clean = tidytext::reorder_within(x = cata_variable_clean,
                                                        by = penalty_lift,
                                                        within = berry)) %>%
  ggplot(mapping = aes(x = cata_variable_clean, y = penalty_lift)) +
  geom_col(aes(fill = penalty_lift), color = "white") + 
  facet_wrap(~berry, scales = "free", nrow = 1) + 
  coord_flip() + 
  tidytext::scale_x_reordered()


## ----removing the legend----------------------------------------------------------------------------------------------------------------------------------------------------------------------
berry_penalty_analysis_data %>%
  mutate(cata_variable_clean = tidytext::reorder_within(x = cata_variable_clean,
                                                        by = penalty_lift,
                                                        within = berry)) %>%
  ggplot(mapping = aes(x = cata_variable_clean, y = penalty_lift)) +
  geom_col(aes(fill = penalty_lift), color = "white") + 
  facet_wrap(~berry, scales = "free", nrow = 1) + 
  coord_flip() + 
  tidytext::scale_x_reordered() +
  theme(axis.title.y = element_blank(),
        legend.position = "none")


## ----removing axis labels---------------------------------------------------------------------------------------------------------------------------------------------------------------------
berry_penalty_analysis_data %>%
  mutate(cata_variable_clean = tidytext::reorder_within(x = cata_variable_clean,
                                                        by = penalty_lift,
                                                        within = berry)) %>%
  ggplot(mapping = aes(x = cata_variable_clean, y = penalty_lift)) +
  geom_col(aes(fill = penalty_lift), color = "white") + 
  facet_wrap(~berry, scales = "free", nrow = 1) + 
  coord_flip() + 
  tidytext::scale_x_reordered() +
  labs(x = NULL, y = "Penalty",
       title = "Penalty / Lift Analysis",
       subtitle = "displays the mean difference (within berries) for when a CATA variable is checked")


## ----final walkthrough of penalty analysis, fig.width = 7-------------------------------------------------------------------------------------------------------------------------------------
berry_penalty_analysis_data %>%
  mutate(cata_variable_clean = tidytext::reorder_within(x = cata_variable_clean,
                                                        by = penalty_lift,
                                                        within = berry)) %>%
  ggplot(mapping = aes(x = cata_variable_clean, y = penalty_lift)) +
  geom_col(aes(fill = penalty_lift), color = "white") + 
  facet_wrap(~berry, scales = "free", nrow = 1) + 
  tidytext::scale_x_reordered() + 
  coord_flip() + 
  theme_classic() + 
  scale_fill_gradient(low = "tan", high = "darkgreen") + 
  labs(title = "Penalty / Lift Analysis",
       subtitle = "displays the mean difference (within berries) for when a CATA variable is checked") +
  theme(axis.title = element_blank(),
        legend.position = "none")


## ----worse cider CA example, fig.width = 7----------------------------------------------------------------------------------------------------------------------------------------------------
draft_cider_plot <- ca_cider_coords %>%
  ggplot(mapping = aes(x = `Dim 1`, y = `Dim 2`, color = type)) + 
  geom_point() +
  ggrepel::geom_text_repel(mapping = aes(label = name)) +
  theme_bw() + 
  scale_color_manual(values = c("darkorange", "darkgreen"))

draft_cider_plot


## ----matrix of eigenvalues and intertia from FactoMineR---------------------------------------------------------------------------------------------------------------------------------------
ca_cider$eig #the second column has the percentage of variance explained by each dimension


## ----list of axis names with str_c------------------------------------------------------------------------------------------------------------------------------------------------------------
str_c(rownames(ca_cider$eig),
      " (", # you'll need to add the symbols yourself, including spaces
      round(ca_cider$eig[, 2], 1),
      "%)")

# Or you can do it one at a time:
str_c("Dimension 1 (",
      round(ca_cider$eig[1, 2], 1),
      "% of intertia)")
str_c("Dimension 2 (",
      round(ca_cider$eig[2, 2], 1),
      "% of intertia)")


## ----making wordy axis labels-----------------------------------------------------------------------------------------------------------------------------------------------------------------
draft_cider_plot +
  labs(x = str_c("Dimension 1, ", round(ca_cider$eig[1, 2], 1), "% of inertia"),
       y = str_c("Dimension 2, ", round(ca_cider$eig[2, 2], 1), "% of inertia"),
       subtitle = "Correspondence Analysis biplot (symmetric)",
       #if you want to insert a line break, you can use `\n`:
       title = "Effect of cider serving temperature on consumer sensory perception\nbased on CATA data")


## ----using geom_vline and geom_hline at the end-----------------------------------------------------------------------------------------------------------------------------------------------
draft_cider_plot +
  theme_void() +
  geom_vline(xintercept = 0, color = "grey", linewidth = 1) +
  geom_hline(yintercept = 0, color = "grey", linewidth = 1)


## ----using geom_vline and geom_hline at the beginning-----------------------------------------------------------------------------------------------------------------------------------------
ca_cider_coords %>%
  ggplot(mapping = aes(x = `Dim 1`, y = `Dim 2`, color = type)) +
  geom_vline(xintercept = 0) +
  geom_hline(yintercept = 0) + 
  geom_point() +
  ggrepel::geom_text_repel(mapping = aes(label = name)) +
  theme_void()


## ----making axes equally scaled---------------------------------------------------------------------------------------------------------------------------------------------------------------
ca_cider_coords %>%
  ggplot(mapping = aes(x = `Dim 1`, y = `Dim 2`, color = type)) +
  geom_vline(xintercept = 0) +
  geom_hline(yintercept = 0) + 
  geom_point() +
  ggrepel::geom_text_repel(mapping = aes(label = name)) +
  theme_bw() +
  coord_equal()


## ----plotting different datasets with different geoms-----------------------------------------------------------------------------------------------------------------------------------------
ca_cider_coords %>%
  ggplot(mapping = aes(x = `Dim 1`, y = `Dim 2`, color = type)) +
  geom_point(data = ca_cider_coords %>% filter(type == "row")) +
  ggrepel::geom_text_repel(mapping = aes(label = name)) +
  theme_bw()


## ----messy legend example---------------------------------------------------------------------------------------------------------------------------------------------------------------------
ca_cider_coords %>%
  ggplot(mapping = aes(x = `Dim 1`, y = `Dim 2`, color = type)) +
  geom_point(aes(shape = Temperature),
             data = ca_cider_coords %>% filter(type == "row")) +
  ggrepel::geom_text_repel(mapping = aes(label = name)) +
  theme_bw()


## ----excluding geoms from the legend----------------------------------------------------------------------------------------------------------------------------------------------------------
ca_cider_coords %>%
  ggplot(mapping = aes(x = `Dim 1`, y = `Dim 2`, color = type)) +
  geom_point(aes(shape = Temperature),
             data = ca_cider_coords %>% filter(type == "row"),
             show.legend = FALSE) +
  ggrepel::geom_text_repel(mapping = aes(label = name)) +
  theme_bw()


## ----excluding aesthetics from the legend-----------------------------------------------------------------------------------------------------------------------------------------------------
ca_cider_coords %>%
  ggplot(mapping = aes(x = `Dim 1`, y = `Dim 2`, color = type)) +
  geom_point(aes(shape = Temperature),
             data = ca_cider_coords %>% filter(type == "row")) +
  ggrepel::geom_text_repel(mapping = aes(label = name)) +
  theme_bw() +
  guides(shape = guide_legend(),
         color = "none")


## ----naive approach to fontface, error=TRUE---------------------------------------------------------------------------------------------------------------------------------------------------
ca_cider_coords %>%
  ggplot(mapping = aes(x = `Dim 1`, y = `Dim 2`, color = type)) + 
  geom_point() +
  ggrepel::geom_text_repel(mapping = aes(label = name, fontface = type)) +
  theme_bw() + 
  scale_color_manual(values = c("darkorange", "darkgreen"))


## ----ggplot fontfaces the right way-----------------------------------------------------------------------------------------------------------------------------------------------------------
ca_cider_coords %>%
  mutate(font = if_else(type == "row", "plain", "italic")) %>%
  ggplot(mapping = aes(x = `Dim 1`, y = `Dim 2`, color = type)) + 
  geom_point() +
  ggrepel::geom_text_repel(mapping = aes(label = name, fontface = font)) +
  theme_bw() + 
  scale_color_manual(values = c("darkorange", "darkgreen"))


## ----final walkthrough of correspondence analysis---------------------------------------------------------------------------------------------------------------------------------------------
nice_cider_labels <-
  labs(x = str_c("Dimension 1, ", round(ca_cider$eig[1, 2], 1), "% of inertia"),
       y = str_c("Dimension 2, ", round(ca_cider$eig[2, 2], 1), "% of inertia"),
       subtitle = "Correspondence Analysis biplot (symmetric)",
       title = "Effect of cider serving temperature on consumer sensory perception")

ca_cider_termplot <-
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
  theme_bw() +
  theme(legend.position = "bottom") +
  nice_cider_labels + 
  scale_color_manual(values = c("darkorange", "darkgreen")) +
  scale_shape_manual(values = c(8, 16)) +
  guides(shape = guide_legend(),
         color = "none")

ca_cider_termplot

save(ca_cider_termplot, file = "data/ca_cider_termplot.RData")




