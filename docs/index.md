---
title: "Publication-quality data visualizations using the R tidyverse"
author: 
  - Jacob Lahne^[Virginia Tech, jlahne@vt.edu]
  - Kyle Hamilton^[Flavor Linguist, hamilton@flavorlinguist.com]
site: "bookdown::bookdown_site"
documentclass: book
output:
  bookdown::gitbook: default
  #bookdown::pdf_book: default
github-repo: jlahne/pangborn-tutorial-2025
bibliography: pangborn-2025.bib
---

# Introduction and welcome {-}




Welcome to the Sensometrics Workshop "**Publication-quality data visualizations using the R tidyverse**"!

This workshop is going to be conducted not using slides, but through **livecoding**.  That means we are going to run code lines in the console or highlight and run code in scripts and other files.  It is also an opportunity and encouragement for you to follow along.  Along with introducing ourselves for today's workshop, we're going to discuss a bit about how that will work here.

## Introductions {-}

### Kyle Hamilton, PhD {-}
Kyle Hamilton is the Lead Sensory Data Analyst and Trainer at Flavor Linguist, LLC, in the US. Kyle's primary research interest is flavor language, including the ways that people talk about flavors using their own words in different contexts. [Flavor Linguist](https://flavorlinguist.com) offers data analysis services for language and survey data, or training and support for those looking to do it themselves.

### Jacob Lahne, PhD {-}

Jacob Lahne is an Associate Professor of Food Science & Technology at Virginia Tech, in the United States.  He runs the Virginia Tech Sensory Evaluation Laboratory, as well as teaching courses in data analytics and coding for food-science research.  His main research focuses are sensory data-analysis methodologies and investigating the sensory properties of fermented and distilled foods and beverages.

### Elizabeth Clark, PhD {-}

Elizabeth Clark is a Senior Scientist in Sensory & Consumer Sciences at McCormick & Company Inc. — a global leader in flavor operating in two segments across 170 countries and territories. McCormick’s passion for Sensory & Consumer Science has led to published research on a replacement for the Scoville heat method for the sensory determination of pungency in capsicum products (Gillette, Appel, & Leggo, 1984), The Sensory Quality System (SQS): a global quality control solution (King et.al, 2022), the EsSense Profile®— a scientific measurement of the human emotional response to flavor (King & Meiselman, 2010), and The Wellsense Profile™ — a questionnaire to measure consumer wellness with foods (King et.al, 2015). Leveraging her interest in data analytics & coding, Elizabeth is helping McCormick usher in a new era of sensory research geared toward addressing rapidly evolving challenges faced by global food & beverage companies.

## Today's agenda {-}

Today's workshop is going to take ~3 hours, and we'll be covering the following material:

1.  Importing/Exporting/Organizing Data
    1. Reading tabular data
    2. Basic data cleaning
    3. Saving/exporting data
3.  Intro to `ggplot2`
5.  Fine-Tuning Publication-Quality ggplots
    1. Adding layers and geoms (e.g., `ggrepel`)
    2. Plot-builders (e.g., `factoextra`)
    3. Combining plots
    1. Exporting plots
    2. Formatting text
    3. Ordering categorical variables

  
## How we're going to run {-}

This workshop is going to be run with **livecoding**, as noted above.  This means we won't be using slides or a prepared video, but running through code step by step to show how these tools are used in practice.  We encourage **you** to also follow along with livecoding, because the best way to learn coding is to actually do it.

### Recommended approach for livecoding {-}

We recommend that you download the pre-made archive of code and data from the [workshop github repo](https://github.com/jlahne/pangborn-tutorial-2025).  This archive, when unzipped, will have a folder structure and a `.Rproj` file.  We recommend that you close out RStudio, unzip the archive, and double click the `.Rproj` file *in that folder*, which will open a new session of RStudio with proper settings (like the home directory) for the files for this workshop.

In that folder, you will find a `data/` folder with the necessary data for the workshop, and a script named `sensometrics-all-code.R`.  This latter file contains all of the code demonstrated in this workshop for your future reference.  You can also follow along with the code at the [workshop's page hosted on github.io]() (which you're reading right now), and which will remain available after this workshop.

Once you're in RStudio, go to the `File > New File > R Script` menu to open a new script.  Scripts are basically workbooks for you to store sequential lines of code to be run in the `Console`.  It is where you can livecode along!  Even though we are giving you all of the code you need right now, you will learn a lot more if you actively write out the code to follow along, rather than just running the entire code file.

The `Console` is the place to run code that is short and easy to type, or that you're experimenting with.  It will allow you to write a single line of code, and after you hit `return`, `R` will execute the command.  This is great for "interactive programming", but it isn't so great for building up a complex workflow, or for following along with this workshop!

You can write multiple lines of code in your `R` Script, then execute each one in any order (although keeping a logical sequence from top to bottom will help you keep track of what you're doing).  In an `R` script, everything is expected to be valid R code.


``` r
You can't write this in an R script because it is plain text.  This will
cause an error.

# If you want to write text or notes to yourself, use the "#" symbol at the start of 
# every line to "comment" out that line.  You can also put "#" in the middle of
# a line in order to add a comment - everything after will be ignored.

1 + 1 # this is valid R syntax

print("hello world") # this is also valid R syntax
```

To run code from your `R` script, put your cursor on the line you want to run and either hit the run button with the green arrow at the top left or (my preferred method) type `cmd + return` (on Mac) or `ctrl + return` (on PC).

### Dealing with errors {-}

Coding means **making mistakes**.  This is fine--as you will surely see today, we will make a ton of trivial errors and have to fix things on the fly.  If you run into trouble, try looking carefully at what you've done and see if you can see what went wrong.  You can also make use of the help files in `R`.

You can always get help on a particular function by typing `?<search term>`, which will make the help documentation for whatever you've searched for appear. For example, try typing the following to get help for the `sessionInfo()` command:


``` r
?sessionInfo
```

But what if you don't know what to search for?

By typing `??<search term>` you will search **all** help files for the search term.  R will return a list of matching articles to you in the help pane.  This is considerably slower, since it's searching hundreds or thousands of text files.  Try typing `??install` into your console to see how this works.

You will notice that there are two types of results in the help list for install.  The help pages should be familiar.  But what are "vignettes"?  Try clicking on one to find out.

Vignettes are formatted, conversational walkthroughs that are increasingly common (and helpful!) in R packages.  Rather than explaining a single function they usually explain some aspect of a package, and how to use it.  And, even better for our purposes, they are written in R Markdown.  Click the "source" link next to the vignette name in order to see how the author wrote it in R Markdown.  This is a great way to learn new tricks.

While you can find vignettes as we just did, a better way is to use the function `browseVignettes()`.  This opens a web browser window that lists **all** vignettes installed on your computer.  You can then use `cmd`/`ctrl + F` to search using terms in the web browser and quickly find package names, function names, or topics you are looking for.

## PSA: not-knowing is normal! {-}

Above, I mentioned "help files". How do we get help when we (inevitably) run into problems in R? There are a couple steps you will find helpful in the future:

1.  Look up the help file for whatever you're doing. Do this by using the syntax `?<search item>` (for example `?c` gets help on the vector command) as a shortcut on the console.
2.  Search the help files for a term you think is related. Can't remember the command for making a sequence of integers? Go to the "Help" pane in RStudio and search in the search box for "sequence". See if some of the top results get you what you need.
3.  The internet. Seriously. I am not kidding even a little bit. R has one of the most active and (surprisingly) helpful user communities I've ever encountered. Try going to google and searching for "How do I make a sequence of numbers in R?" You will find quite a bit of useful help. I find the following sites particularly helpful
    1.  [Stack Overflow](https://stackoverflow.com/questions/tagged/r)
    2.  [Cross Validated/Stack Exchange](https://stats.stackexchange.com/questions/tagged/r)
    3.  Seriously, [Google will get you most of the way to helpful answers](https://is.gd/80V5zF) for many basic R questions.


We may come back to this, but I want to emphasize that **looking up help is normal**. I do it all the time. Learning to ask questions in helpful ways, how to quickly parse the information you find, and how to slightly alter the answers to suit your particular situation are key skills.

### Getting Help {-}
If you get stuck and can't figure out what went wrong, we are here to help!  Because we have 2 instructors for this workshop, one of us is available to help at any time.

When you run into trouble, please raise your hand.  We'll be keeping an eye out, and whichever instructor isn't livecoding will come to help you.  If your issue is a common one or something we think is worth noting, don't worry--we'll make time to discuss it!

