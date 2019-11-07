# NBA Player Shot Analysis

Brian Yi

## Introduction

**Purpose:** The Golden State Warriors won the 2018 NBA Championship with the help of their leader and 2016 unanimous MVP, Stephen Curry. We are building a model that can analyze the conditions surrounding Steph Curry's shot selection and predict his shot's success rate based on these features. This kind of analysis would be useful to the Golden State Warriors by improving their star player's shooting efficiency. If built properly, this model could help Stephen Curry regain his MVP form and win his organization another championship!

**Method of Approach:** The dataset for this project is extracted from stats.nba.com and can be viewed at the following link: [Data Set Link](https://www.kaggle.com/boonpalipatana/nba-playoff-shots-2018). The NBA 2018 playoff shot data consists of 13,470 observations each with 24 columns that describe the player, shot outcome, shot distance, shot type, minutes remaining etc.

We will build the best logistic regression model that can predict whether Steph Curry makes or misses a shot based on a variety of shot-taking conditions. The features we plan on using to predict Curry's shot `outcome` are: `game_clock`, `shot_distance`, and `home`.

-   `game_clock` is the time in game that Curry takes a shot out of 48 minutes. We want to measure whether Curry becomes more accurate as the game progresses. If he is more accurate in the third and fourth quarters, then maybe he should focus on passing and facilitating the ball earlier on in the ball game.
-   `shot_distance` is the distance from the hoop that Curry takes a shot. Steph Curry is known for his deadly 3-point shooting, so we want to see just how accurate he is far from the hoop. If he is as accurate from behind the 3-point line as he is close to the hoop, shot distance should be insignificant in predicting shot outcome in a logistic regression model.
-   `home` is whether Curry is playing in a home or away game. Usually in sports, having home court advantage with fans cheering for the home team will give them a mental edge throughout the game. We want to see if Steph Curry is affected when he is playing in an away game.

We first create three logistic regression models from varying combinations of these three predictors. Then we evaluate these models using empirical logit plots, Wald tests, and likelihood ratio tests. Finally, we compare models using the nested likelihood ratio test to determine the model that is best at predicting whether Steph Curry will make a shot.

**Results**: Turns out that the best logistic model we built was our final model that uses `shot_distance` and `shot_worth` (variables we added) to predict shot `outcome`. Although Stephen Curry is an amazing 3-point shooter, he still makes layups (shots directly underneath the basket) so accurately that there is still a decreasing trend in efficiency the farther he shoots from the basket. Since `game_clock` and `home` are insignificant predictors, Curry isn't more accurate the longer he is in game and also shoots equally well at an away game as he does at home.
