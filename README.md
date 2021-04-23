IMDB rating prediction
================
by Peter Hontaru

<div style="text-align:center">

<img src="figures/dukesquare_blue.jpg" width="350" height="300">

</div>

## Background

### Problem Statement:

This document is a report from the final course project for the **Linear
regression course**, as part of the **Duke University Statistics with R
course in partnership with Coursera**.

This project is based on a fictitious scenario where I’ve been hired as
a data scientist at Paramount Pictures. The data presents numerous
variables on movies such as audience/critic ratings, number of votes,
runtime, genre, etc. Paramount Pictures is looking to gather insights
into determining the acclaim of a film and other novel patterns or
ideas.

### Summary

-   our linear model was only able to capture 29% of the variability in
    our data
-   the model significantly over-predicts for low IMDB ratings and
    over-predicts for high IMDB ratings
-   while we were able to identify significant predictors, the design of
    the study does not allow for causation
-   critics and audiences tend to differ in their movie taste,
    particularly in categories such as Comedy and Action & Adventure
-   some accolades are better predictors of IMDB ratings than others
-   Friday might be the best day to release a movie in terms of audience
    access, but the movies are not the most popular (as judged by total
    IMDB votes or IMDB rating)
-   there is an exponential relationship between the number of votes a
    movie receives and the IMDB rating, where highly rated movies are
    much more popular than normally expected
-   further optimisation can be made if we have a specific goal to make
    a movie for either critics or audience
    -   similarly, popularity could also be determined by total number
        of votes (quantity) rather than rating (quality)

### Next steps/recommendations:

-   relatively low sample sizes before 1990; our data is not
    representative of each year included in the dataset(but it is
    representative overall)
-   we could use more data on factors we would normally have access
    before the release of the film: budget, actor/director social media
    influence (which might affect ratings or popularity), etc
-   other types of models could be explored, particularly exponential
    ones
-   some techniques were specifically used in the context of the
    course - more robust methods are available such as:
    -   R adjustment, as it is known to be more reliable than an
        arbitrary p value selection
    -   train/test split of our dataset
    -   automated backward/forward model selection (ie. through Caret),
        since a manual approach is susceptible to human error
    -   no data pre-processing

### Dataset

The data set is comprised of 651 randomly sampled movies produced and
released before 2016.

Source:

-   Rotten Tomatoes: <https://www.rottentomatoes.com/>
-   IMDB API: <https://www.imdb.com/>

## Extended analysis

Full project available:

-   **RECOMMENDED: at the following
    [link](https://rpubs.com/PeterHontaru/linear-regression), in HTML
    format**
-   in the **Movie rating.md** file of this repo (however, recommend
    previewing it at the link above since it was originally designed as
    a html document)
-   <http://htmlpreview.github.io/?https://github.com/peterhontaru/IMDB-Movie-Rating-Prediction/blob/main/Movie-rating.html>
