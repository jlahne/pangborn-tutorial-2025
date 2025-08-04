# Tutorial for Pangborn 2025: “Publication-quality data visualizations using the R tidyverse”

This repository contains code and markdown files for the tutorial presented at Pangborn 2025.  We are still tweaking the materials, so if you'd like to download all of the code and other files before the workshop, check back about a week before the conference.

# DO THIS FIRST

We have created [a quick tutorial](https://lhamilton.shinyapps.io/pre-tutorial-setup-2025/) to help you install `R`, RStudio, and to confirm that your installation is functioning for the tutorial _prior_ to Sensometrics.

Please go ❗**[to the tutorial website](https://lhamilton.shinyapps.io/pre-tutorial-setup-2025/)**❗ and follow the instructions there.  You will be guided in installing or updating your software so that, on the day of the tutorial, we can dive right into the material!

# Quick Reference

If you have followed the instructions from the tutorial above, you should have all the material you need for the workshop, and your version of `R` and RStudio should be set up properly. 

If you want to see code from the workshop, see 👉**[the bookdown website](https://jlahne.github.io/pangborn-tutorial-2025/)**👈 for all of the R code broken into individual steps alongside expected output and instructions, or [download and extract (link goes our notes on how to do this)]([https://lhami.github.io/sensometrics-r-tutorial-2024/index.html#recommended-approach-for-livecoding](https://jlahne.github.io/pangborn-tutorial-2025/index.html#how-were-going-to-run)) the [code/pangborn-2025-data-download.zip file](https://github.com/jlahne/pangborn-tutorial-2025/blob/main/code/) for just the lines of R code in a more compact format. Once you've extracted the files, open up RStudio or a basic Notepad program (something that makes `.txt` files) and from the File > Open menu, open the extracted `pangborn-all-code.R` file. If you double-click it from your file viewer, it will run all of the code directly in the console without giving you a chance to read anything.

If you want detailed, written instructions, you can consult the instructions for our [tutorial at Pangborn 2023](https://github.com/lhami/pangborn-r-tutorial-2023), which gave detailed set up instructions for `R`, RStudio, and a similar set of tutorials.

# What this tutorial will cover

>In this tutorial, we will introduce the audience to ggplot2 and the rest of the tidyverse R packages with the aim of developing sufficient basic skills to visualize multivariate sensory and consumer data. We will provide a learning dataset for the analysis—a set of free response comments and overall liking scores from a central location test on berries. We will teach participants how to import, manipulate, and plot data using user-friendly, “tidy” R programming. All resources used in the tutorial are open-source and will remain available to attendees, including an R script covering the full workflow.
>
>At the end of the tutorial, attendees will be able to prepare raw sensory data for common multivariate visual representations in R.


# What you should already know

The main prerequisite for this tutorial is that you [**have followed the tutorial setup instructions**](https://lhamilton.shinyapps.io/pre-tutorial-setup-2025/).

We expect participants to have basic familiarity with data types, variables, functions, and installing/using packages in R/RStudio. Basic understanding of statistics is helpful but not required. In general, participants who have some experience with `R` or coding for research will have a better time.

In order to follow along with this tutorial, you will need to know how to install, open, and run commands in R or RStudio. It will also be helpful to know:
- [How to store and access data in objects (aka variables, casually) is](https://datacarpentry.github.io/R-genomics/01-intro-to-R.html#creating_objects)
- [How to use a function](https://datacarpentry.github.io/R-genomics/01-intro-to-R.html#functions)
- What the common R [data types (logical, numeric, character)](https://datacarpentry.github.io/R-genomics/01-intro-to-R.html#vectors_and_data_types) and [structures (vector, factor, data frame, matrix)](https://datacarpentry.github.io/R-genomics/02-starting-with-data.html) are
- [How to install and load packages](https://rladiessydney.org/courses/01-basicbasics-2)
- [How to chain multiple functions together in a readable fashion with pipes (`|>` or `%>%`)](https://www.r-bloggers.com/2021/05/the-new-r-pipe/)

If you're worried about being able to follow along, you might benefit from taking a look at some of the [sources below](#how-can-you-get-ahead-or-learn-more).  These range from self-paced tutorials to full how-to books.

# What we will *not* be covering:

* Statistical theory (including for Correspondence Analysis)
* How to write these kinds of web-friendly documents and bookdowns
* Production programming for R
* All possible data types
* Basic concepts of computing, for example file storage, version control, data types and storage, etc.

In order to make graphs with your own data, in addition to the skills we'll explicitly cover during this workshop, you'll need to be able to "wrangle" your initial data into a format that's ready to run your analysis. There are many ways to do this. If you already know how to reshape data in R (or how to do it in excel and then import the file), then whatever way you're already comfortable with should be great! If you have no idea where to start reformatting your data or you're frustrated with the way you've done it in the past, we personally think it's easier to learn how to wrangle data, write code, and remember what you've done later with the R tidyverse. The [updated R opus by Jacob Lahne](https://jlahne.github.io/r-opus-v2/) has a brief intro to the tidyverse in chapter 1 and many examples of data reshaping for common analyses in sensory science throughout, and the book [R for Data Science](https://r4ds.hadley.nz/) is an excellent general purpose introduction to data wrangling and data science in the R tidyverse. Both are available for free online.

# How can you get ahead or learn more?

Much of the material we have used to develop this workshop came from the fantastic, open-science and open-coding community that has developed around R.  If you find yourself wanting to learn more, I highly recommend starting with the following sources:

*  [Software Carpentry's R for Social Scientists](https://datacarpentry.org/r-socialsci/)
*  [R for Data Science](https://r4ds.had.co.nz/)
*  [Stat 545](https://stat545.com/)
*  [Correspondence Analysis in Practice](https://doi.org/10.1201/9781315369983/)
*  [Text Mining with R](https://www.tidytextmining.com/)
*  [Data Visualization: A Practical Introduction](https://socviz.co/)

Beyond that, remember the power of searching forums like Stackoverflow and Stackexchange--looking at a few related threads can usually teach you how to solve your problem, as well as develop your critical-thinking and problem-solving skills for the future.
