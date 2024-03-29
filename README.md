# World languages with dual vs. singular/plural
Data from [GramBank](https://grambank.clld.org/parameters/GB031#2/21.0/151.9) with individual language data with lon/lat coordinates were reverse geocoded to 2023 country boundaries using tidygeocoder. The countries were then manually coded as island nations vs. not. The same country might have speakers of a number of different languages, and sometimes both languages with and without dual form. <br><br>
Each point in each panel represents one <i>language</i>, not country. The points were organized in circles using the packcircles package, after which a function was run to swap a proportion of the point coordinates close to the boundary between the categories to add a bit of visual noise.

![Alt text](dual_viz.png)
