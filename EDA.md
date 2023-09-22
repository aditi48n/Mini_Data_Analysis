Exploratory Data Analysis
================
Aditi Nagaraj Nallan
2023-09-22

## Load the required packages below.

``` r
suppressPackageStartupMessages(library(datateachr))
suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(plotly))
suppressPackageStartupMessages(library(lubridate))
suppressPackageStartupMessages(library(MetBrewer))
```

## Data Sourcing

A number of public data sets acquired from different sources have been
compiled and made available as part of the `datateachr` package. The
data set that I am working with for this Mini Data Analysis is called
**Vancouver Trees**. This data set has been acquired courtesy of The
[City of Vancouver’s Open Data
Portal](https://opendata.vancouver.ca/pages/home/). It currently has
146611 rows and 20 columns. The data set is continually updated as and
when new trees are planted in the city. The reason I chose this data set
is that it is of great value to urban planners and city hall
authorities. It will be interesting to explore this data set and
generate reports that will benefit these groups of individuals.

Some other features of the data set are highlighted below:

``` r
# Dim: shows the dimensions of the data (rows x columns)
dim(vancouver_trees)
```

    ## [1] 146611     20

``` r
# Glimpse: A transposed form where columns run down the page, and data runs across making it possible to see every column in a data frame.
glimpse(vancouver_trees)
```

    ## Rows: 146,611
    ## Columns: 20
    ## $ tree_id            <dbl> 149556, 149563, 149579, 149590, 149604, 149616, 149…
    ## $ civic_number       <dbl> 494, 450, 4994, 858, 5032, 585, 4909, 4925, 4969, 7…
    ## $ std_street         <chr> "W 58TH AV", "W 58TH AV", "WINDSOR ST", "E 39TH AV"…
    ## $ genus_name         <chr> "ULMUS", "ZELKOVA", "STYRAX", "FRAXINUS", "ACER", "…
    ## $ species_name       <chr> "AMERICANA", "SERRATA", "JAPONICA", "AMERICANA", "C…
    ## $ cultivar_name      <chr> "BRANDON", NA, NA, "AUTUMN APPLAUSE", NA, "CHANTICL…
    ## $ common_name        <chr> "BRANDON ELM", "JAPANESE ZELKOVA", "JAPANESE SNOWBE…
    ## $ assigned           <chr> "N", "N", "N", "Y", "N", "N", "N", "N", "N", "N", "…
    ## $ root_barrier       <chr> "N", "N", "N", "N", "N", "N", "N", "N", "N", "N", "…
    ## $ plant_area         <chr> "N", "N", "4", "4", "4", "B", "6", "6", "3", "3", "…
    ## $ on_street_block    <dbl> 400, 400, 4900, 800, 5000, 500, 4900, 4900, 4900, 7…
    ## $ on_street          <chr> "W 58TH AV", "W 58TH AV", "WINDSOR ST", "E 39TH AV"…
    ## $ neighbourhood_name <chr> "MARPOLE", "MARPOLE", "KENSINGTON-CEDAR COTTAGE", "…
    ## $ street_side_name   <chr> "EVEN", "EVEN", "EVEN", "EVEN", "EVEN", "ODD", "ODD…
    ## $ height_range_id    <dbl> 2, 4, 3, 4, 2, 2, 3, 3, 2, 2, 2, 5, 3, 2, 2, 2, 2, …
    ## $ diameter           <dbl> 10.00, 10.00, 4.00, 18.00, 9.00, 5.00, 15.00, 14.00…
    ## $ curb               <chr> "N", "N", "Y", "Y", "Y", "Y", "Y", "Y", "Y", "Y", "…
    ## $ date_planted       <date> 1999-01-13, 1996-05-31, 1993-11-22, 1996-04-29, 19…
    ## $ longitude          <dbl> -123.1161, -123.1147, -123.0846, -123.0870, -123.08…
    ## $ latitude           <dbl> 49.21776, 49.21776, 49.23938, 49.23469, 49.23894, 4…

Before jumping into any kind of analysis, let us further explore the
dataset and perform any required data cleaning.

## Data Cleaning and further exploration

Data cleaning is performed to get rid of any irregularities in the form
of missing values, incorrect format, incorrect headers or anomalies and
outliers.

``` r
# Find the missing data: Using the 'Tidyverse' package in R first we convert the vancouver_trees dataframe to a boolean (True/False) dataframe based on missing values using the 'summarise_all' function. Then we convert the dataframe to long format for ease of counting the number of missing values using pivot_longer. Following this using the count function, we count the number of missing values for each column variable and store results in Answer1.1. 

#Reference: Class worksheets

Answer1 = vancouver_trees  %>%
  summarise_all(list(~is.na(.)))%>%
  pivot_longer(everything(),
               names_to = "Columns", values_to="Data_missing") %>%
  count(Columns, Data_missing) 
```

    ## Warning: Returning more (or less) than 1 row per `summarise()` group was deprecated in
    ## dplyr 1.1.0.
    ## ℹ Please use `reframe()` instead.
    ## ℹ When switching from `summarise()` to `reframe()`, remember that `reframe()`
    ##   always returns an ungrouped data frame and adjust accordingly.
    ## ℹ The deprecated feature was likely used in the dplyr package.
    ##   Please report the issue at <https://github.com/tidyverse/dplyr/issues>.
    ## This warning is displayed once every 8 hours.
    ## Call `lifecycle::last_lifecycle_warnings()` to see where this warning was
    ## generated.

``` r
# Filter out only the missing data from Answer1
missing_data = Answer1 %>%
  filter(Data_missing == TRUE)

#Plot the number of missing values using ggplot2 package in R: First using the filter function from the tidyverse package filter out columns containing missing values. Then using ggplot and geom_bar plot the number of missing values on y axis versus column names on x axis. Specify plot title and axis titles. 
ggplot(missing_data, aes(x = Columns, y = 1, size = n, text = n)) +
  geom_point(aes(color = Columns), alpha = 0.6) +
  scale_size_continuous(guide = "none") +  
  theme_minimal() +
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        legend.position = "none") + 
  labs(x = 'Column Name', 
       y = "", 
       title = 'Bubble Plot of Missing Values in Vancouver Trees Dataset')
```

![](EDA_files/figure-gfm/unnamed-chunk-3-1.png)<!-- -->

From the above plot we can see that missing data is present in 5
columns. Out of these five columns we have no use for the cultivar_name
and the plant_area, so we can remove those columns from the data set
along with other columns which we will not be using for further
analysis. For the remaining three columns we will have to perform
imputation or remove missing values depending on how they both affect
the downstream analysis.

``` r
#First lets start by checking the missing data columns for the percentage of missing data
percentage_missing = missing_data %>%
  left_join(Answer1, by = "Columns") %>%
  filter(Data_missing.x == TRUE & Data_missing.y == FALSE) %>%
  mutate(percentage_missing = (n.x / (n.x + n.y)) * 100) %>%
  select(Columns, percentage_missing)

print(percentage_missing)
```

    ## # A tibble: 5 × 2
    ##   Columns       percentage_missing
    ##   <chr>                      <dbl>
    ## 1 cultivar_name              46.1 
    ## 2 date_planted               52.2 
    ## 3 latitude                   15.5 
    ## 4 longitude                  15.5 
    ## 5 plant_area                  1.01

``` r
#Then we Remove all unwanted columns and clean the dataframe
vancouver_trees_cleaned = vancouver_trees %>%
  select(-civic_number, -assigned, -root_barrier, -cultivar_name, 
         -plant_area, -on_street_block, -std_street, -street_side_name, -curb)
```

From the output above we can see that *date_planted* has roughly 52%
missing data, simply removing these values will affect our downstream
analysis especially if we are trying to study the distribution of trees
planted in the last 10 years or such. Therefore, to fill in the missing
date lets perform imputation by Proportional Random Sampling.

Proportional Random Sampling allows us to sample dates from the existing
dates in the data set proportionally. Thereby ensuring that the overall
distribution of dates in the data set remains the same after imputation.

``` r
# Calculate the distribution of the existing dates
date_distribution = vancouver_trees_cleaned %>%
  filter(!is.na(date_planted)) %>%
  count(date_planted) %>%
  mutate(proportion = n / sum(n))

# Impute missing dates using proportional random sampling
vancouver_trees_date_imputed = vancouver_trees_cleaned %>%
  rowwise() %>%
  mutate(date_planted = ifelse(is.na(date_planted),
                              sample(date_distribution$date_planted, 
                                     size = 1, 
                                     prob = date_distribution$proportion),
                              date_planted)) %>%
  ungroup()

vancouver_trees_cleaned$date_planted = as.Date(vancouver_trees_cleaned$date_planted)
vancouver_trees_date_imputed$date_planted = as.Date(vancouver_trees_date_imputed$date_planted, origin = "1970-01-01")

# Combine the datasets with a new column indicating origin
combined_data = vancouver_trees_cleaned %>%
  mutate(Data_Type = "Original") %>%
  bind_rows(vancouver_trees_date_imputed %>% mutate(Data_Type = "Imputed"))

# Plot the distribution
ggplot(combined_data, aes(x = date_planted, fill = Data_Type)) +
  geom_histogram(position = "identity", alpha = 0.5, bins = 30) +
  labs(title = "Distribution of Dates: Original vs. Imputed", 
       x = "Date", y = "Count") +
  theme_minimal() +
  scale_fill_manual(values = c("Original" = "blue", "Imputed" = "red"))
```

    ## Warning: Removed 76548 rows containing non-finite values (`stat_bin()`).

![](EDA_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->

From the distribution above we can see that both the original dates with
the missing values and the new imputed dates follow a similar
distribution. Let us now go on to impute the latitude and logitude
columns.

Both of these columns contain only 15% missing data and can therefore be
removed but since we already have the neighborhood and street name on
which the trees have been planted, we can use the location entries from
these rows to impute the missing values.

``` r
vancouver_trees_date_imputed = vancouver_trees_date_imputed %>%
  mutate(address = case_when(
    on_street == "TO BE DETERMINED" ~ paste(neighbourhood_name, "VANCOUVER", sep = ", "),
    TRUE ~ paste(on_street, neighbourhood_name, "VANCOUVER", sep = ", ")
  ))

#Create a table of unique addresses from the dataframe
address_lookup = vancouver_trees_date_imputed %>%
  filter(!is.na(latitude) & !is.na(longitude)) %>%
  group_by(address) %>%
  summarise(
    latitude_avg = mean(latitude, na.rm = TRUE),
    longitude_avg = mean(longitude, na.rm = TRUE)
  ) %>%
  ungroup()

#Impute missing values
vancouver_trees_imputed = vancouver_trees_date_imputed %>%
  left_join(address_lookup, by = "address") %>%
  mutate(
    latitude = if_else(is.na(latitude), latitude_avg, latitude),
    longitude = if_else(is.na(longitude), longitude_avg, longitude)
  ) %>%
  select(-latitude_avg, -longitude_avg)

Answer1 = vancouver_trees_imputed  %>%
  summarise_all(list(~is.na(.)))%>%
  pivot_longer(everything(),
               names_to = "Columns", values_to="Data_missing") %>%
  count(Columns, Data_missing)

missing_data = Answer1 %>%
  filter(Data_missing == TRUE)

percentage_missing = missing_data %>%
  left_join(Answer1, by = "Columns") %>%
  filter(Data_missing.x == TRUE & Data_missing.y == FALSE) %>%
  mutate(percentage_missing = (n.x / (n.x + n.y)) * 100) %>%
  select(Columns, percentage_missing)

print(percentage_missing)
```

    ## # A tibble: 2 × 2
    ##   Columns   percentage_missing
    ##   <chr>                  <dbl>
    ## 1 latitude               0.709
    ## 2 longitude              0.709

From the table above we can see that we have brought down the missing
data in latitude and longitude from around 15% to less than 1%. Some
neighborhoods were not found and so we will retain these entries with
their missing values.

## Univariate Analysis

Now that we have our cleaned data set, let us perform a univariate
analysis to see the number of trees planted across the different
neighborhoods in Vancouver.

``` r
vancouver_trees_imputed = vancouver_trees_imputed %>%
  mutate(age = round(abs(as.numeric(difftime(Sys.Date(), date_planted, units = "days")) / 365.25)))

total_trees_overall = nrow(vancouver_trees_imputed)

trees_by_neighbourhood = vancouver_trees_imputed %>%
  group_by(neighbourhood_name) %>%
  summarise(total_trees = n()) %>%
  arrange(-total_trees)

ggplot(trees_by_neighbourhood, aes(x = total_trees, y = reorder(neighbourhood_name, -total_trees))) +
  geom_bar(stat = "identity", fill = "navyblue") +
  labs(title = "Total Number of Trees Planted by Neighbourhood",
       x = "Total Number of Trees",
       y = "Neighbourhood") +
  theme_minimal() 
```

![](EDA_files/figure-gfm/unnamed-chunk-7-1.png)<!-- -->

We see that **Renfrew-Collingwood** has the highest number of trees
planted.

Next let us see the top ten most planted tree types in Vancouver.

``` r
trees_by_name = vancouver_trees_imputed %>%
  group_by(common_name) %>%
  summarise(total_trees = n()) %>%
  arrange(-total_trees)

top_10_trees = trees_by_name %>% top_n(10, wt = total_trees)

ggplot(top_10_trees, aes(x = total_trees, y = reorder(common_name, -total_trees))) +
  geom_bar(stat = "identity", fill = "lightblue") +
  labs(title = "Top 10 Most Planted Trees",
       x = "Total Number of Trees",
       y = "Tree Type") +
  theme_minimal() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  ) 
```

![](EDA_files/figure-gfm/unnamed-chunk-8-1.png)<!-- -->

We see that **Kwanzan flowering cherry** is the most planted tree type
in Vancouver with over 10,000 trees.

## Bivariate Analysis

Given the vast number of **Kwanzan flowering cherry trees** in
Vancouver, lets see if there is any relationship between the age of
these trees and their diameter across the different neighborhoods. The
general assumption being that as trees get older, they might also have a
larger diameter, depending on various environmental and genetic factors.

``` r
filtered_data = vancouver_trees_imputed %>%
  filter(common_name == "KWANZAN FLOWERING CHERRY")

ggplot(data = filtered_data, aes(x = factor(age), y = diameter, color = neighbourhood_name)) +
  geom_boxplot() +
  labs(title = "Kwanzan flowering cherry tree distribution by Age and Diameter",
       x = "Age", y = "Diameter") +
  theme_minimal(base_size = 18) +  # Base text size
  theme(
    legend.position = "none",
    axis.title = element_text(size = 25),
    axis.text.x = element_blank(),
    axis.text.y = element_text(size = 24),           # Axis label size
    strip.text = element_text(size = 20),          # Facet label size
    plot.title = element_text(size = 24, hjust = 0.5)) + scale_color_manual(values=met.brewer("Signac", 22)) + facet_wrap(~neighbourhood_name, scales = "free_x") 
```

![](EDA_files/figure-gfm/plot1-1.png)<!-- -->

From the plots above we can see that the Kwanzan flowering cherry trees
are distributed across the neighborhoods irrespective of their age and
diameter and majority of these trees have a diameter under 50 inches,
regardless of age, with only a few exceptions. To further quantify this
relationship, let’s compute the correlation coefficient between age and
diameter below.

``` r
correlation_results = filtered_data %>%
  group_by(neighbourhood_name) %>%
  summarize(correlation = cor(age, diameter, use = "complete.obs"), 
            .groups = "drop") 

correlation_results
```

    ## # A tibble: 22 × 2
    ##    neighbourhood_name       correlation
    ##    <chr>                          <dbl>
    ##  1 ARBUTUS-RIDGE                 0.135 
    ##  2 DOWNTOWN                      0.223 
    ##  3 DUNBAR-SOUTHLANDS             0.0399
    ##  4 FAIRVIEW                      0.144 
    ##  5 GRANDVIEW-WOODLAND            0.0896
    ##  6 HASTINGS-SUNRISE              0.228 
    ##  7 KENSINGTON-CEDAR COTTAGE      0.120 
    ##  8 KERRISDALE                    0.0845
    ##  9 KILLARNEY                    -0.0444
    ## 10 KITSILANO                     0.0624
    ## # ℹ 12 more rows

From the table above we can see that, most neighborhoods show a weak
positive correlation between tree age and diameter indicating that as
trees get older, their diameter tends to increase slightly.The
neighborhood with the strongest positive correlation is
Hastings-Sunrise. However, even this correlation is only weak to
moderate.

## Multivariate Analysis

Now that we have looked at the relationship between age and diameter for
the most commonly planted tree and found that there is only a weak
correlation between the two, let’s identify trends and variations in
tree ages and diameters across different neighborhoods and tree types.
This was accomplished for the top 10 most planted trees.

``` r
filtered_data = vancouver_trees_imputed %>%
  filter(common_name %in% top_10_trees$common_name)

#Fit a two-way ANOVA model and include the main effects of neighborhood and tree type and their interaction 
anova_model_age = aov(age ~ neighbourhood_name * common_name, data = filtered_data)
print(summary(anova_model_age))
```

    ##                                   Df  Sum Sq Mean Sq F value   Pr(>F)    
    ## neighbourhood_name                21    3713     177   3.283 5.28e-07 ***
    ## common_name                        9   80489    8943 166.034  < 2e-16 ***
    ## neighbourhood_name:common_name   188   49267     262   4.865  < 2e-16 ***
    ## Residuals                      46202 2488602      54                     
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
ggplot(filtered_data, aes(x = neighbourhood_name, y = age, color = common_name)) +
  stat_summary(fun = mean, geom = "point") +  # Plot mean points
  stat_summary(fun = mean, geom = "line", aes(group = common_name)) +
  labs(title = "Interaction between Neighbourhood and Tree Type on Age",
       y = "Mean Age", x = "Neighborhood") + scale_color_manual(values=met.brewer("Hokusai1", 10)) + theme_minimal() + 
  theme(legend.position = "right",
    axis.text.x = element_text(angle = 90, hjust = 1)) +
  guides(color = guide_legend(title = "Tree Type"))
```

![](EDA_files/figure-gfm/plot2-1.png)<!-- -->

From the ANOVA summary table and plot we can see that there is a
significant difference in tree age across neighborhoods and tree
types.The relationship between tree age and tree type changes depending
on the neighborhood (and vice versa), since the interaction term is
significant. The trees mostly fall in the age range of 18 to 22 years.

``` r
filtered_data = vancouver_trees_imputed %>%
  filter(common_name %in% top_10_trees$common_name)

#Fit a two-way ANOVA model and include the main effects of neighborhood and tree type and their interaction 
anova_model_diameter = aov(diameter ~ neighbourhood_name * common_name, data = filtered_data)
print(summary(anova_model_diameter))
```

    ##                                   Df  Sum Sq Mean Sq F value Pr(>F)    
    ## neighbourhood_name                21   87540    4169   100.1 <2e-16 ***
    ## common_name                        9 1249113  138790  3331.7 <2e-16 ***
    ## neighbourhood_name:common_name   188  140949     750    18.0 <2e-16 ***
    ## Residuals                      46202 1924644      42                   
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
ggplot(filtered_data, aes(x = neighbourhood_name, y = diameter, color = common_name)) +
  stat_summary(fun = mean, geom = "point") +  # Plot mean points
  stat_summary(fun = mean, geom = "line", aes(group = common_name)) +
  labs(title = "Interaction between Neighbourhood and Tree Type on Diameter",
       y = "Mean Diameter", x = "Neighborhood") + scale_color_manual(values=met.brewer("Hokusai1", 10)) + theme_minimal() + 
  theme(legend.position = "right",
    axis.text.x = element_text(angle = 90, hjust = 1)) +
  guides(color = guide_legend(title = "Tree Type"))
```

![](EDA_files/figure-gfm/plot3-1.png)<!-- -->

From the ANOVA summary table and plot we can see that there is a
significant difference in tree diameter across neighborhoods and tree
types.The relationship between tree diameter and tree type changes
depending on the neighborhood (and vice versa), since the interaction
term is significant.

## Conclusion and Next steps

Given that both Age and Diameter showed significant differences across
neighborhoods and tree types, further pairwise comparisons will help in
identifying where exactly these differences lie. Additionally, other
columns in the data set can be considered for downstream exploratory
analysis such as tree height, location etc.
