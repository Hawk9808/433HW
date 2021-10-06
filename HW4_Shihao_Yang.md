HW4\_Shihao\_Yang
================
Shihao Yang
10/4/2021

# My github repo: (<https://github.com/Hawk9808/433HW>)

``` r
orig_data <- flights
# summary(orig_data)
```

What time of day should you fly if you want to avoid delays as much as
possible? Does this choice depend on anything? Season? Weather? Airport?
Airline? Find three patterns (null results are ok\!).

## Intro

> *In my following analysis, I choose three variables-Airport, Season,
> Visibility-to study the relationship between them and the total delay
> time in a day. From the result of my analysis, I conclude that we
> should avoid to fly at around 20:00 to avoid the flight delays. And
> the earlier the time our flight scheduled to depart, the less possible
> our flight delay.*

#### Data Preparing

> *For the data preparing, firstly, I calculate the total delay time and
> I make the date with year, month, and day variable. Then I create
> season variable in order to study if there is some relationship
> between the delay time and season. *

``` r
weather1 = weather %>%  
  mutate(date = make_date(year,month,day)) %>% 
  group_by(date, origin) %>% 
  summarise(min_vis=min(visib),
            mean_vis = mean(visib))
```

    ## `summarise()` has grouped output by 'date'. You can override using the `.groups` argument.

``` r
# planes1 = planes
# View(weather)
flight2<-orig_data %>% 
  group_by(hour) %>% 
  mutate(total_delay = dep_delay + arr_delay,
         date = lubridate::make_datetime(year, month, day),
         season = case_when(month == 12 | month == 1 |month == 2 ~ "Winter",
                            month == 3 | month == 4 |month == 5 ~ "Spring",
                            month == 6 | month == 7 |month == 8 ~ "Summer",
                            month == 9 | month == 10 |month == 11 ~ "Autumn")) %>% 
  left_join(weather1) %>% 
  select(origin, date, hour, total_delay, season, mean_vis) %>% 
  drop_na()
```

    ## Joining, by = c("origin", "date")

``` r
delay_origin <- flight2 %>% 
  group_by(origin) %>% 
  summarise(mean_delay = mean(total_delay),
            hour = hour,
            total_delay = total_delay)
```

    ## `summarise()` has grouped output by 'origin'. You can override using the `.groups` argument.

``` r
ggplot(data = delay_origin, aes(x = hour, y = total_delay) ) + 
  geom_smooth()+
  facet_wrap(~origin + mean_delay)
```

    ## `geom_smooth()` using method = 'gam' and formula 'y ~ s(x, bs = "cs")'

![](HW4_Shihao_Yang_files/figure-gfm/unnamed-chunk-3-1.png)<!-- -->

> *As we can see from the above plot, the peak of the total delay time
> is around 20:00. And the delay time do seem much larger in EWR than in
> JFK and LGA as we can see the mean delay time which I put under the
> airport name in the plot. *

``` r
delay_season <- flight2 %>% 
  group_by(season) %>% 
  summarise(mean_delay = mean(total_delay),
            hour = hour,
            total_delay = total_delay)
```

    ## `summarise()` has grouped output by 'season'. You can override using the `.groups` argument.

``` r
ggplot(data = delay_season, aes(x = hour, y = total_delay) ) + 
  geom_smooth()+
  facet_wrap(~season + mean_delay)
```

    ## `geom_smooth()` using method = 'gam' and formula 'y ~ s(x, bs = "cs")'

![](HW4_Shihao_Yang_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->

> *From the above graph, we can still conclude that the peak of the
> delay time happens around 20:00. Also, we can see the possibility that
> season have effect on the delay time. The average delay time in Autumn
> is much less than other three seasons and the delay time in summer is
> the largest among four.*

``` r
ggplot(data = flight2, aes(x = mean_vis, y = total_delay) ) + 
  geom_smooth(se = F, method = "lm")+
  facet_grid(~hour)
```

    ## `geom_smooth()` using formula 'y ~ x'

![](HW4_Shihao_Yang_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->

> *As we can see from the above analysis, as the visibility score
> increase, the delay time decrease. And after we connect all the mid
> point in all facets, we can still conclude that the peak delay time
> happens at around 20:00 in a tipical day.*

Write your results into Rmarkdown. Include a short introduction that
summarizes the three results. Then, have a section for each finding.
Support each finding with data summaries and visualizations. Include
your code when necessary.
