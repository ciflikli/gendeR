# gendeR

LSE IR Gender Project

https://gokhan.shinyapps.io/gender/

Exploratory analysis of ~13,000 scholarly works included in International Relations (IR) Department syllabi in 2016-2017 at the London School of Economics and Political Science. The main aim of this project is to quantify the publication bias in academia. The findings mirror the US Ivy League patterns of a roughly 80/20 male-to-female author ratio.

R packages utilised: ```shinydashboard, shinyjs, dygraphs, sunburstR, DT, htmlwidgets, RColorBrewer, bubbles, rbokeh.```

# Introduction

* The first plot displays the raw count of publications by female authors included in a reading list by publication year:

![](/img/plot.png)

Within this dataset, we see that works by female authors are more likely to be included as time passes. This is hardly surprising, as prior to 1960's academic authorship was more or less exclusively a male enterprise. However, when plotted side by side with male authors, we find that the trend is universal:

![](/img/intro.png)

# Time-Series

* This interactive time-series chart of author gender by year shows both genders in the same graph with percentages:

![](/img/ts.png)

# Publication Pathways

* In order to better understand the pathways leading to reading list inclusion, we create and plot sequences involving at least one female author. The sequences follow the order Decade > Article/Book > Top/Other Publisher > Single/Co-Authored > Female/Male Co-Author. The numbers inside the dial report the count and overall percentage of the current selection:

![](/img/sunburst.png)

# Course Breakdown

* We visualise all 43 courses (18 Undergraduate-level, 23 Master's-level, and 2 PhD-level) included in the dataset. We cluster all courses under five overarching categories: Security/Statecraft Studies, Regional Studies, International Political Economy, Theory, and International Organisations/Law. Hovering over a course displays additional information such as course convener rank, gender, and the total number of publications from that course:

![](/img/bokeh.png)

# Logistic Link

* This graph calculates a logistic function based on selected parameters: female author percentage and starting year. The first setting sets the threshold for creating a dummy variable and the latter subsets the data. For example, the default settings display which publication-years since 1960 has at least 20% female authors:

![](/img/logit.png)

# Co-Authorship

* Finally, we visualise co-authorship patters using three parameters: maximum number of authors, maximum number of female authors, ad maximum number of male authors. The bubbles are recalculated each time a slider is updated and it can be animated. The radii are calculated using square root to offset the male single-author dominance:

![](/img/bubbles.png)

# Publisher Info

* This interactive dataframe allows for searching for specific publishers as well filtering on female author ratio and the total number of publications:

![](/img/pub.png)
