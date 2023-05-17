# Wrap-up and further resources



Let's look back at what we were aiming to do today:

> In this tutorial, we will introduce the audience to the R statistical programming environment and the RStudio Interactive Development Environment (IDE) with the aim of developing sufficient basic skills to conduct a text analysis on sensory-relevant text data. We will provide a learning dataset of text data for the analysis—a set of food-product free-comment reviews that are associated with overall liking scores. This will allow us to demonstrate connections between text analysis methods and basic sensory and consumer science approaches. We will also provide an R script that walks through all steps of importing, manipulating, and analysing the test dataset.
>
> The tutorial will have 2 sections. In the first section of the tutorial, we will introduce R and RStudio, we will cover the basic commands of R, and we will cover key, user-friendly conventions of ”tidy” R programming for importing, manipulating, and plotting data using the “tidyverse” packages. In the second section we will use the “tidytext” package to conduct basic text analysis, including text tokenization, text modeling using TF-IDF, and basic lexicon-based sentiment analysis.

We have managed to touch on all of these topics, but of course we have taken the most cursory look at each.  I hope what we've gone over today has inspired you, sure, but I mostly hope it has shown you **how much you can do with just a little knowledge**.  My journey in terms of learning data science with `R`, and text analysis in particular, has been all about building my coding ability incrementally.  My code looks more like this than anything else, but I am able to get so much done:

<center>

![What does good code even look like? (via [XKCD](https://xkcd.com/844/))](https://imgs.xkcd.com/comics/good_code.png)

</center>

By developing your ability to code (in `R` or Python, or whatever works for you--Julia?) you will open up a whole set of analyses that you would otherwise be unable to access.

## Getting help

1.  Look up the help file for whatever you're doing. Do this by using the syntax `?<search item>` (for example `?c` gets help on the vector command) as a shortcut on the console.
2.  Search the help files for a term you think is related. Can't remember the command for making a sequence of integers? Go to the "Help" pane in RStudio and search in the search box for "sequence". See if some of the top results get you what you need.
3.  The internet. Seriously. I am not kidding even a little bit. R has one of the most active and (surprisingly) helpful user communities I've ever encountered. Try going to google and searching for "How do I make a sequence of numbers in R?" You will find quite a bit of useful help. I find the following sites particularly helpful
    1.  [Stack Overflow](https://stackoverflow.com/questions/tagged/r)
    2.  [Cross Validated/Stack Exchange](https://stats.stackexchange.com/questions/tagged/r)
    3.  Seriously, [Google will get you most of the way to helpful answers](https://is.gd/80V5zF) for many basic R questions.

I want to emphasize that **looking up help is normal**. I do it all the time. Learning to ask questions in helpful ways, how to quickly parse the information you find, and how to slightly alter the answers to suit your particular situation are key skills.

## Learning more with Sensometrics Society

This workshop was organized and sponsored by the Sensometrics Society.  Want to learn more?  We are hosting our biennial conference **online 15-17 November, 2022**:

<center>

![Sensometrics Society biannual conference banner](img/sensometrics-logo-2022.png)

</center>

In general, Sensometrics has a focus on methods and skills for the analysis of sensory data.  If you're interested in contributing to this focus (or just participating), please see our call for papers:

> Please submit an abstract for either an oral or poster presentation on the conference website at [www.sensometrics2022.com](http://sensometric.org/2022). All submissions need to be received by **October 17, 2022**. Notification about acceptance will be made by October 24, 2022. All accepted contributions are invited to submit a full paper for inclusion in a virtual special issue of Food Quality and Preference (FQAP). 

You can contact us for more information or with questions at [sensometrics2022@gmail.com](mailto:sensometrics2022@gmail.com).

## Further reading/resources

1.  General `R` programming
    1.  [Data Carpentry's *R for Social Scientists*](https://datacarpentry.org/r-socialsci/) (and, really the courses from [The Carpentries](https://carpentries.org/) in general)
    1.  [Wickham & Grolemund's *R for Data Science*](https://r4ds.had.co.nz/)
    2.  [The stat545 course website](https://stat545.com/)
    2.  [Healy's *Data Visualization*](https://socviz.co/)
    3.  My own (somewhat opinionated and eccentric) course from VT: [FST 5984](https://jlahne.github.io/food-data-science-2022/)
2.  Text analysis
    1.  [Jurafsky & Martin's seminal textbook, *Speech & Language Processing*](https://web.stanford.edu/~jurafsky/slp3/)
    2.  [Silge & Robinson's *Text Mining with R*](https://www.tidytextmining.com/)
    3.  [Chollet et al.'s *Deep Learning with R*](https://www.manning.com/books/deep-learning-with-r-second-edition)
  
## Questions/Comments

If you end up being confused, come talk to me, or email me at **jlahne at vt dot edu**.  I'd love to learn about what you're working on! 
