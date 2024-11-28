# L06 - logicals and numbers ----
# Stat 301-1

## load packages ----
# Loading package(s)
library(tidyverse)
library(nycflights13)
library(naniar)

# load data
college_rankings <- read_csv("data/college_rankings.csv")

## Exercises ----

### Ex 1 ----

# Use `mutate()`, `is.na()`, and `count()` together to describe how the
# missing values in `dep_time`, `scheduled_dep_time`, and `dep_delay` are connected.

flights |> 
  mutate(
    miss_dep_time = is.na(dep_time),
    miss_sched_dep_time = is.na(sched_dep_time),
    miss_dep_delay = is.na(dep_delay),
    .keep = "none"
  ) |> 
  count(miss_dep_time, miss_sched_dep_time, miss_dep_delay) |> 
  mutate(
    prop = n / sum(n),
    pct = 100 * prop)

### Ex 2 ----

# Find all flights where `arr_delay` is missing but `dep_delay` is not. Find all 
# flights where neither `arr_time` nor `sched_arr_time` is missing, but `arr_delay` is.

flights |> filter(is.na(arr_delay) == "TRUE" & is.na(dep_delay) == "FALSE") |> 
  relocate(arr_delay, dep_delay)

flights |> filter(is.na(arr_time) == "FALSE" &
                    is.na(sched_arr_time) == "FALSE" &
                    is.na(arr_delay) == "TRUE") |> 
            select(arr_time, sched_arr_time, arr_delay)


### Ex 3---

n_miss(flights$dep_time)

flights |> 
  filter(is.na(dep_time) == "TRUE")

### Ex 4----

# What will `sum(is.na(x))` tell you? How about `mean(is.na(x))`?

sum(is.na(flights$dep_delay))
# shows the total number of missing values of a variable

mean(is.na(flights$dep_delay))
# shows the proportion of missing values of a variable

### Ex 5 ----

# A number is even if it’s divisible by two, which in R you can find out with `x %% 2 == 0`. 
# Use this fact and `if_else()` to determine whether each number between 0 and 20 is even or odd.

if_else(c(0:20) %% 2 == 0, "EVEN", "ODD")

### Ex 6 ----
flights |> count(dest, sort = TRUE)

flights |> count(tailnum, wt = distance)


# `group_by(), summarize()`, and `arrange()

flights |> 
  group_by(dest) |> 
  summarize(n = n()) |> 
  arrange(desc(n))

flights |> 
  group_by(tailnum) |> 
  summarize(n = n()) 

### Ex 7 ----

# a. For each destination, compute the total minutes of delay. 

not_canceled <- flights |> 
  filter(!is.na(arr_delay)) 


not_canceled |> 
  ggplot(aes(arr_delay)) +
  geom_boxplot() +
  coord_cartesian(xlim = c(-60, 60))

not_canceled |> 
  filter(arr_delay > 0) |> 
  summarize(
    total_arr_delay = sum(arr_delay),
    mean_arr_delay = mean(arr_delay),
    med_arr_delay = median(arr_delay),
    n_flights = n(),
    .by = dest
  ) |> 
  arrange(desc(med_arr_delay))

# b. For each `flight`, compute the proportion of the total delay for its destination.

not_canceled |> 
  filter(arr_delay > 0) |> 
  summarize(
    total_dest = sum(arr_delay),
    .by = c(flight, dest)
  ) |> 
  mutate(
    prop_arr_delay = total_dest / sum(total_dest),
    .by = flight
  ) |> 
  arrange(flight)

### Case Study ----



# - Use `case_when()` to define each observation as either "Midwest", "Northeast", "South", or "West" as defined by the US Census Bureau. Call this new variable `region`.

midwest <- c("WI", "IL", "ND", "SD", "NE", "MN", "KS","MI", "OH", "IN", "IA", "MO")

northeast <- c("PA", "NJ", "CT", "NY", "RI", "MA", "VT", "NH", "ME")

south <- c("DE", "MD", "WV", "DC", "VA", "KY", "TN", "NC", "SC",
           "GA", "FL", "AL", "MS", "AR", "LA", "OK", "TX")

west <- c("CA", "OR", "WA", "ID", "MT", "WY", "UT", "CO", "NV", "AZ", "NM")

college_rankings <- college_rankings |> 
  mutate(
    region =   case_when(
      state_abbr %in% midwest ~ "Midwest",
      state_abbr %in% northeast ~ "Northeast",
      state_abbr %in% south ~ "South",
      state_abbr %in% west ~ "West"
    )
  )

# - Use an `ifelse` statement to change the `public` variable to be `TRUE` if it is public and `FALSE` if it is not.

college_rankings <- college_rankings |> 
  mutate(public = ifelse(public == "public", TRUE, FALSE))
  
# - Use the `cut` function to categorize the `overall_score` into bins spanning increments of 10.

college_rankings$overall_score <- as.numeric(college_rankings$overall_score)

college_breaks <- seq(0, 100, by = 10)

college_rankings$overall_score <- 
  cut(college_rankings$overall_score, breaks = college_breaks, right = FALSE)

# Round `tuition_fees` to the nearest $1,000.

college_rankings$tuition_fees <-
  round(college_rankings$tuition_fees, digits = -3)
