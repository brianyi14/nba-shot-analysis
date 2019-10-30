NBA Player Shot Analysis
================
Brian Yi

Introduction
============

In this project, we will build the best logistic regression model trained on data collected from the 2018 NBA playoff shot logs. Our model can then be used to predict how likely a shot will go into the basketball hoop based on a variety of shot-taking conditions. *Our focus is not to drop any poor models we initially find but to rather use them for comparison to explore why certain models may be better than others and why!*

The dataset for this project is extracted from stats.nba.com and can be viewed at the following link: [Data Set Link](https://www.kaggle.com/boonpalipatana/nba-playoff-shots-2018). The NBA 2018 playoff shot data consists of 13,470 observations each with 24 columns describing the player, shot outcome, shot distance, shot type, minutes remaining etc.

Since the GoldenState Warriors won the 2018 NBA playoffs, we will focus on the contributions of their team leader and 2016 unanimous MVP, Stephen Curry. We want to analyze and determine the factors that make his shooting so deadly on the court. The features we will consider to predict Curry's `shot_outcome` are: `game_clock`, `shot_distance`, and `home`.

    -`game_clock` is the time of the game that Curry takes a shot out of 48 minutes. We want to measure whether Curry becomes more accurate as the game progresses.
    -`shot_distance` is the distance from the hoop that Curry takes a shot. Steph Curry is known for 3-point shots so we want to see if he is more accurate the farther he is away from the hoop (note: NBA regulation 3-point line is approximately 22-23 ft depending on which part of the arc). 
    -`home` is whether a game is a home game or away game. Usually in sports, a home game with the fans cheering for their team will give the home team a mental edge throughout the game. We want to see if Steph Curry shoots just as well on the road in an away game as he is at home.

We will create logistic regression models from varying combinations of these three predictors. Then we will compare these models to determine which is best in predicting whether Steph Curry will make a shot in the 2018 NBA playoffs.
