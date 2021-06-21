ST\_558\_NHL\_Project
================
Pramodini Karwande
6/13/2021

-   [ST558 : National Hockey League(NHL) Data
    Analysis](#st558--national-hockey-leaguenhl-data-analysis)
    -   [List of Required libraries](#list-of-required-libraries)
    -   [NHL Records API Functions](#nhl-records-api-functions)
    -   [NHL Stats API Function](#nhl-stats-api-function)
    -   [Exploratory Data Analysis
        (EDA)](#exploratory-data-analysis-eda)
        -   [Create new variables](#create-new-variables)
        -   [Contingency Tables](#contingency-tables)
        -   [Numerical summaries for some quantitative
            variables](#numerical-summaries-for-some-quantitative-variables)
    -   [PLOTS :](#plots-)
        -   [1) Bar plot : Active/inactive players Bar
            plot](#1-bar-plot--activeinactive-players-bar-plot)
        -   [2) Histogram Plot : Histogram Plot for goli 12 games played
            and win
            rate.](#2-histogram-plot--histogram-plot-for-goli-12-games-played-and-win-rate)
        -   [3) Scatterplot : Games Played vs Win
            Rate](#3-scatterplot--games-played-vs-win-rate)
        -   [4) ScatterP Plot : Games played vs
            Wins](#4-scatterp-plot--games-played-vs-wins)
        -   [5) Box Plot: Boxplot of games played and losses by
            individual
            players](#5-box-plot-boxplot-of-games-played-and-losses-by-individual-players)
        -   [6) Histogram Plot : Histogram for games
            Played](#6-histogram-plot--histogram-for-games-played)

# ST558 : National Hockey League(NHL) Data Analysis

NHL is professional *ice hokey league* in the world and is one of the
major professional sports league in the United States.We will be doing
data analysis using NHL records api and NHL stats api. Project 1
involves creating a **vignette** and summerizing data from **NHL API**.

Below in the knitr options, I have set echo true to show the code work,
evaluation true and hided the warning and messages which appears due to
including required libraries, or if any function got deprecated and
suggest to use valid options, etc…

## List of Required libraries

“httr” : httr is designed to map closely to the underlying http
protocol.  
“jsonlite” : The jsonlite package is a JSON parser/generator optimized
for the web. Its main strength is that it implements a bidirectional
mapping between JSON data and the most important R data types.  
“dplyr” : dplyr provides the set of data manipulation tools, functions
like group\_by, summarise,…  
“kableExtra” : kableExtra is a light weight table generator coming from
‘knitr’.  
“knitr” : knitr provides full control of the output without heavy coding
work. I used for options to show the code chunks from RMarkdown and hide
the warnings, messages generating from code with one liner coding.  
“summarytools”: I used summarytools to fetch min, max, Q1,… data  
“ggplot2” : Using ggplot2, I able to generate scatter plot, boxplot,
histogram, bar chart for EDA

``` r
# section that notes the required packages needed to run the code
library("httr")
library("jsonlite")
library("dplyr")
library("kableExtra")
library("knitr")
library("summarytools")
library("ggplot2")
```

## NHL Records API Functions

Below are the functions to contact the NHL records API for the endpoints
listed below. The functions are accessing API and displaying the part of
data here for the reference.The user have the option to specify the
franchise of choice by both name and ID number.

Below in the code, I declared variables which are having the common
value which can be used through out in this program. Used paste0
function to join the string to make url with common string (like base
url) and requirement specific string (like franchise ID). Used kable
function which helped to generate and style HTML table. Verified the
null values if we are getting null value from user.

``` r
#Grab base URL for API
base_url <- "https://records.nhl.com/site/api"
urlExtention <- "?cayenneExp=franchiseId="
reqID <- ""
team_stat_url <- "https://statsapi.web.nhl.com/api/v1/teams"
team_extend_url <- "?expand=team.stats"

getNhlData<- function(franchiseID=NULL, franchiseName=NULL, tab_name=NULL) {
reqID <-  getID(franchiseID, franchiseName, tab_name)

if(tab_name == "franchise-detail")
{
  urlExtention <- "?cayenneExp=mostRecentTeamId="
}

if(!is.null(reqID)) {
  full_url <- paste0(base_url, "/", tab_name,urlExtention,reqID)
}
else {
  if((tab_name == "franchise-team-totals") || (tab_name == "franchise") ) {
    full_url <- paste0(base_url, "/", tab_name)
  }
}

# GET request to API
res <- GET(full_url)

franchise_data <- fromJSON(rawToChar(res$content), flatten = TRUE)
franchise_data <- data.frame(franchise_data)

if(tab_name == "franchise") {
  franchiseDataTab <- franchise_data %>% mutate("Team"=  paste0(data.teamPlaceName," ",data.teamCommonName)) %>% select("ID" = data.id, "First Season Id"= data.firstSeasonId, "Last Season Id"= data.lastSeasonId, Team)
  #franchiseDataTab <- na.omit(franchiseDataTab)
  knitr::kable(head(franchiseDataTab), caption = tab_name) %>% kable_styling()
}
else {
  knitr::kable(head(franchise_data), caption = tab_name) %>% kable_styling()
}
}

# verify if user provided ID or franchise name or both are null.
getID <- function(franchiseID=NULL, franchiseName=NULL, tab_name=NULL){
  retVal <- NULL
  if (!is.null(franchiseID)) {
    retVal<- franchiseID
    return(retVal)
  }
  else if (!is.null(franchiseName)) {
    retVal <- getName(franchiseName)
    return(retVal)
  }
  else {
    return(retVal)
  }
}

# map Name to ID to get franchise data.
getName <- function(franchiseName=NULL) {
    tab_name <- "franchise"
    full_url <- paste0(base_url, "/", tab_name)
    res <- GET(full_url)

    franchiseNameData <- fromJSON(rawToChar(res$content), flatten = TRUE)
    franchiseNameData <- data.frame(franchiseNameData)
    franchiseID <- franchiseNameData %>% filter(data.fullName == franchiseName) %>% select(data.id)

    return(franchiseID)
} 
```

## NHL Stats API Function

Below is a function to contact the NHL stats API for the
?expand=team.stats modifier. The function is able to take a single team
or return data from all teams. Here, API url is different for all teams
and individual player. Verified it using the if, else function and
redirect API call accordingly.

``` r
getNHLTeamStatsData <- function(teamID=NULL) {
  if (!is.null(teamID)) {
    team_stat_full_url <- paste0(team_stat_url,"/",teamID,team_extend_url)
  }
  else {
    team_stat_full_url <- paste0(team_stat_url,team_extend_url)
  }
  
  res <- GET(team_stat_full_url)

    team_stat_data <- fromJSON(rawToChar(res$content), flatten = TRUE)
    team_stat_data <- data.frame(team_stat_data)
    knitr::kable(head(team_stat_data), caption = "NHL Team Stat Data") %>% kable_styling()
}

# *****************TESTING START***********************
 #getNHLTeamStatsData(NULL)
 #getNHLTeamStatsData(3)
# *****************TESTING END***********************
```

\#Wrapper Function Wrapper function : that is essentially a
one-stop-shop for the user to access any of the API endpoints listed for
the project study. This function simply calls the appropriate endpoint
as per the users request (including any modifiers, teamIDs, etc.). This
is achieved using switch case based on user request. All the requests
call is done and tested based on ST 558- Project.

``` r
nhlWrapper <- function(nhlDataRequest, franchiseID=NULL,franchiseName=NULL, tab_name=NULL) {
  output = switch (nhlDataRequest,
    "/franchise"                = getNhlData(franchiseID,franchiseName,"franchise"),
    "/franchise-team-totals"    = getNhlData(franchiseID,franchiseName,"franchise-team-totals"),
    "/franchise-season-records" = getNhlData(franchiseID,NULL,"franchise-season-records"),
    "/franchise-goalie-records" = getNhlData(franchiseID,NULL,"franchise-goalie-records"),
    "/franchise-skater-records" = getNhlData(franchiseID,NULL,"franchise-skater-records"),
    "/franchise-detail"         = getNhlData(franchiseID,NULL,"franchise-detail"),
    "/individual-team-stat"     = getNHLTeamStatsData(teamID = 2),
    "/teams-stat"               = getNHLTeamStatsData(NULL),
    )
  return(output)
}

# *****************TESTING START***********************

nhlWrapper("/franchise",NULL, NULL, "franchise" )
```

<table class="table" style="margin-left: auto; margin-right: auto;">
<caption>
franchise
</caption>
<thead>
<tr>
<th style="text-align:right;">
ID
</th>
<th style="text-align:right;">
First Season Id
</th>
<th style="text-align:right;">
Last Season Id
</th>
<th style="text-align:left;">
Team
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
19171918
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:left;">
MontrÃ©al Canadiens
</td>
</tr>
<tr>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
19171918
</td>
<td style="text-align:right;">
19171918
</td>
<td style="text-align:left;">
Montreal Wanderers
</td>
</tr>
<tr>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
19171918
</td>
<td style="text-align:right;">
19341935
</td>
<td style="text-align:left;">
St. Louis Eagles
</td>
</tr>
<tr>
<td style="text-align:right;">
4
</td>
<td style="text-align:right;">
19191920
</td>
<td style="text-align:right;">
19241925
</td>
<td style="text-align:left;">
Hamilton Tigers
</td>
</tr>
<tr>
<td style="text-align:right;">
5
</td>
<td style="text-align:right;">
19171918
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:left;">
Toronto Maple Leafs
</td>
</tr>
<tr>
<td style="text-align:right;">
6
</td>
<td style="text-align:right;">
19241925
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:left;">
Boston Bruins
</td>
</tr>
</tbody>
</table>

``` r
nhlWrapper("/franchise-team-totals",NULL, NULL, "franchise-team-totals" )
```

<table class="table" style="margin-left: auto; margin-right: auto;">
<caption>
franchise-team-totals
</caption>
<thead>
<tr>
<th style="text-align:right;">
data.id
</th>
<th style="text-align:right;">
data.activeFranchise
</th>
<th style="text-align:right;">
data.firstSeasonId
</th>
<th style="text-align:right;">
data.franchiseId
</th>
<th style="text-align:right;">
data.gameTypeId
</th>
<th style="text-align:right;">
data.gamesPlayed
</th>
<th style="text-align:right;">
data.goalsAgainst
</th>
<th style="text-align:right;">
data.goalsFor
</th>
<th style="text-align:right;">
data.homeLosses
</th>
<th style="text-align:right;">
data.homeOvertimeLosses
</th>
<th style="text-align:right;">
data.homeTies
</th>
<th style="text-align:right;">
data.homeWins
</th>
<th style="text-align:right;">
data.lastSeasonId
</th>
<th style="text-align:right;">
data.losses
</th>
<th style="text-align:right;">
data.overtimeLosses
</th>
<th style="text-align:right;">
data.penaltyMinutes
</th>
<th style="text-align:right;">
data.pointPctg
</th>
<th style="text-align:right;">
data.points
</th>
<th style="text-align:right;">
data.roadLosses
</th>
<th style="text-align:right;">
data.roadOvertimeLosses
</th>
<th style="text-align:right;">
data.roadTies
</th>
<th style="text-align:right;">
data.roadWins
</th>
<th style="text-align:right;">
data.shootoutLosses
</th>
<th style="text-align:right;">
data.shootoutWins
</th>
<th style="text-align:right;">
data.shutouts
</th>
<th style="text-align:right;">
data.teamId
</th>
<th style="text-align:left;">
data.teamName
</th>
<th style="text-align:right;">
data.ties
</th>
<th style="text-align:left;">
data.triCode
</th>
<th style="text-align:right;">
data.wins
</th>
<th style="text-align:right;">
total
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
19821983
</td>
<td style="text-align:right;">
23
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
2993
</td>
<td style="text-align:right;">
8902
</td>
<td style="text-align:right;">
8792
</td>
<td style="text-align:right;">
525
</td>
<td style="text-align:right;">
85
</td>
<td style="text-align:right;">
96
</td>
<td style="text-align:right;">
790
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
1211
</td>
<td style="text-align:right;">
169
</td>
<td style="text-align:right;">
44773
</td>
<td style="text-align:right;">
0.5306
</td>
<td style="text-align:right;">
3176
</td>
<td style="text-align:right;">
686
</td>
<td style="text-align:right;">
84
</td>
<td style="text-align:right;">
123
</td>
<td style="text-align:right;">
604
</td>
<td style="text-align:right;">
84
</td>
<td style="text-align:right;">
78
</td>
<td style="text-align:right;">
196
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:left;">
New Jersey Devils
</td>
<td style="text-align:right;">
219
</td>
<td style="text-align:left;">
NJD
</td>
<td style="text-align:right;">
1394
</td>
<td style="text-align:right;">
105
</td>
</tr>
<tr>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
19821983
</td>
<td style="text-align:right;">
23
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
257
</td>
<td style="text-align:right;">
634
</td>
<td style="text-align:right;">
697
</td>
<td style="text-align:right;">
53
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
74
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
120
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
4266
</td>
<td style="text-align:right;">
0.0039
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
67
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
63
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
25
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:left;">
New Jersey Devils
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:left;">
NJD
</td>
<td style="text-align:right;">
137
</td>
<td style="text-align:right;">
105
</td>
</tr>
<tr>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
19721973
</td>
<td style="text-align:right;">
22
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
3788
</td>
<td style="text-align:right;">
11907
</td>
<td style="text-align:right;">
12045
</td>
<td style="text-align:right;">
678
</td>
<td style="text-align:right;">
84
</td>
<td style="text-align:right;">
170
</td>
<td style="text-align:right;">
963
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
1587
</td>
<td style="text-align:right;">
166
</td>
<td style="text-align:right;">
57792
</td>
<td style="text-align:right;">
0.5133
</td>
<td style="text-align:right;">
3889
</td>
<td style="text-align:right;">
909
</td>
<td style="text-align:right;">
82
</td>
<td style="text-align:right;">
177
</td>
<td style="text-align:right;">
725
</td>
<td style="text-align:right;">
70
</td>
<td style="text-align:right;">
86
</td>
<td style="text-align:right;">
177
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:left;">
New York Islanders
</td>
<td style="text-align:right;">
347
</td>
<td style="text-align:left;">
NYI
</td>
<td style="text-align:right;">
1688
</td>
<td style="text-align:right;">
105
</td>
</tr>
<tr>
<td style="text-align:right;">
4
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
19721973
</td>
<td style="text-align:right;">
22
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
310
</td>
<td style="text-align:right;">
899
</td>
<td style="text-align:right;">
986
</td>
<td style="text-align:right;">
53
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
95
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
139
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
5693
</td>
<td style="text-align:right;">
0.0129
</td>
<td style="text-align:right;">
8
</td>
<td style="text-align:right;">
86
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
76
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
12
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:left;">
New York Islanders
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:left;">
NYI
</td>
<td style="text-align:right;">
171
</td>
<td style="text-align:right;">
105
</td>
</tr>
<tr>
<td style="text-align:right;">
5
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
19261927
</td>
<td style="text-align:right;">
10
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
6560
</td>
<td style="text-align:right;">
20020
</td>
<td style="text-align:right;">
20041
</td>
<td style="text-align:right;">
1143
</td>
<td style="text-align:right;">
76
</td>
<td style="text-align:right;">
448
</td>
<td style="text-align:right;">
1614
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
2716
</td>
<td style="text-align:right;">
153
</td>
<td style="text-align:right;">
86129
</td>
<td style="text-align:right;">
0.5127
</td>
<td style="text-align:right;">
6727
</td>
<td style="text-align:right;">
1573
</td>
<td style="text-align:right;">
77
</td>
<td style="text-align:right;">
360
</td>
<td style="text-align:right;">
1269
</td>
<td style="text-align:right;">
68
</td>
<td style="text-align:right;">
79
</td>
<td style="text-align:right;">
408
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:left;">
New York Rangers
</td>
<td style="text-align:right;">
808
</td>
<td style="text-align:left;">
NYR
</td>
<td style="text-align:right;">
2883
</td>
<td style="text-align:right;">
105
</td>
</tr>
<tr>
<td style="text-align:right;">
6
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
19261927
</td>
<td style="text-align:right;">
10
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
518
</td>
<td style="text-align:right;">
1447
</td>
<td style="text-align:right;">
1404
</td>
<td style="text-align:right;">
104
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
137
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
266
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
8181
</td>
<td style="text-align:right;">
0.0000
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
162
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
7
</td>
<td style="text-align:right;">
107
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
44
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:left;">
New York Rangers
</td>
<td style="text-align:right;">
8
</td>
<td style="text-align:left;">
NYR
</td>
<td style="text-align:right;">
244
</td>
<td style="text-align:right;">
105
</td>
</tr>
</tbody>
</table>

``` r
nhlWrapper("/franchise-season-records",3, NULL, "franchise-season-records" )
```

<table class="table" style="margin-left: auto; margin-right: auto;">
<caption>
franchise-season-records
</caption>
<thead>
<tr>
<th style="text-align:right;">
data.id
</th>
<th style="text-align:left;">
data.fewestGoals
</th>
<th style="text-align:left;">
data.fewestGoalsAgainst
</th>
<th style="text-align:left;">
data.fewestGoalsAgainstSeasons
</th>
<th style="text-align:left;">
data.fewestGoalsSeasons
</th>
<th style="text-align:left;">
data.fewestLosses
</th>
<th style="text-align:left;">
data.fewestLossesSeasons
</th>
<th style="text-align:left;">
data.fewestPoints
</th>
<th style="text-align:left;">
data.fewestPointsSeasons
</th>
<th style="text-align:left;">
data.fewestTies
</th>
<th style="text-align:left;">
data.fewestTiesSeasons
</th>
<th style="text-align:left;">
data.fewestWins
</th>
<th style="text-align:left;">
data.fewestWinsSeasons
</th>
<th style="text-align:right;">
data.franchiseId
</th>
<th style="text-align:left;">
data.franchiseName
</th>
<th style="text-align:right;">
data.homeLossStreak
</th>
<th style="text-align:left;">
data.homeLossStreakDates
</th>
<th style="text-align:right;">
data.homePointStreak
</th>
<th style="text-align:left;">
data.homePointStreakDates
</th>
<th style="text-align:right;">
data.homeWinStreak
</th>
<th style="text-align:left;">
data.homeWinStreakDates
</th>
<th style="text-align:right;">
data.homeWinlessStreak
</th>
<th style="text-align:left;">
data.homeWinlessStreakDates
</th>
<th style="text-align:right;">
data.lossStreak
</th>
<th style="text-align:left;">
data.lossStreakDates
</th>
<th style="text-align:right;">
data.mostGameGoals
</th>
<th style="text-align:left;">
data.mostGameGoalsDates
</th>
<th style="text-align:right;">
data.mostGoals
</th>
<th style="text-align:right;">
data.mostGoalsAgainst
</th>
<th style="text-align:left;">
data.mostGoalsAgainstSeasons
</th>
<th style="text-align:left;">
data.mostGoalsSeasons
</th>
<th style="text-align:right;">
data.mostLosses
</th>
<th style="text-align:left;">
data.mostLossesSeasons
</th>
<th style="text-align:right;">
data.mostPenaltyMinutes
</th>
<th style="text-align:left;">
data.mostPenaltyMinutesSeasons
</th>
<th style="text-align:right;">
data.mostPoints
</th>
<th style="text-align:left;">
data.mostPointsSeasons
</th>
<th style="text-align:right;">
data.mostShutouts
</th>
<th style="text-align:left;">
data.mostShutoutsSeasons
</th>
<th style="text-align:right;">
data.mostTies
</th>
<th style="text-align:left;">
data.mostTiesSeasons
</th>
<th style="text-align:right;">
data.mostWins
</th>
<th style="text-align:left;">
data.mostWinsSeasons
</th>
<th style="text-align:right;">
data.pointStreak
</th>
<th style="text-align:left;">
data.pointStreakDates
</th>
<th style="text-align:right;">
data.roadLossStreak
</th>
<th style="text-align:left;">
data.roadLossStreakDates
</th>
<th style="text-align:right;">
data.roadPointStreak
</th>
<th style="text-align:left;">
data.roadPointStreakDates
</th>
<th style="text-align:right;">
data.roadWinStreak
</th>
<th style="text-align:left;">
data.roadWinStreakDates
</th>
<th style="text-align:right;">
data.roadWinlessStreak
</th>
<th style="text-align:left;">
data.roadWinlessStreakDates
</th>
<th style="text-align:right;">
data.winStreak
</th>
<th style="text-align:left;">
data.winStreakDates
</th>
<th style="text-align:right;">
data.winlessStreak
</th>
<th style="text-align:left;">
data.winlessStreakDates
</th>
<th style="text-align:right;">
total
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:right;">
36
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:left;">
St. Louis Eagles
</td>
<td style="text-align:right;">
5
</td>
<td style="text-align:left;">
Jan 08 1931 - Feb 03 1931
</td>
<td style="text-align:right;">
12
</td>
<td style="text-align:left;">
Dec 20 1922 - Feb 28 1923
</td>
<td style="text-align:right;">
10
</td>
<td style="text-align:left;">
Dec 30 1922 - Feb 28 1923, Nov 28 1925 - Jan 28 1926
</td>
<td style="text-align:right;">
9
</td>
<td style="text-align:left;">
Dec 11 1930 - Feb 03 1931
</td>
<td style="text-align:right;">
9
</td>
<td style="text-align:left;">
Dec 06 1930 - Jan 01 1931, Jan 08 1931 - Feb 03 1931
</td>
<td style="text-align:right;">
12
</td>
<td style="text-align:left;">
Jan 21 1920 - QBD 1 @ SEN 12, Mar 07 1921 - HAM 5 @ SEN 12
</td>
<td style="text-align:right;">
138
</td>
<td style="text-align:right;">
144
</td>
<td style="text-align:left;">
1934-35 (48)
</td>
<td style="text-align:left;">
1929-30 (44)
</td>
<td style="text-align:right;">
31
</td>
<td style="text-align:left;">
1934-35 (48)
</td>
<td style="text-align:right;">
619
</td>
<td style="text-align:left;">
1926-27 (44)
</td>
<td style="text-align:right;">
64
</td>
<td style="text-align:left;">
1926-27 (44)
</td>
<td style="text-align:right;">
15
</td>
<td style="text-align:left;">
1925-26 (36), 1927-28 (44)
</td>
<td style="text-align:right;">
13
</td>
<td style="text-align:left;">
1928-29 (44)
</td>
<td style="text-align:right;">
30
</td>
<td style="text-align:left;">
1926-27 (44)
</td>
<td style="text-align:right;">
12
</td>
<td style="text-align:left;">
Jan 24 1928 - Feb 25 1928
</td>
<td style="text-align:right;">
7
</td>
<td style="text-align:left;">
Nov 17 1934 - Dec 09 1934
</td>
<td style="text-align:right;">
8
</td>
<td style="text-align:left;">
Nov 18 1926 - Dec 28 1926
</td>
<td style="text-align:right;">
5
</td>
<td style="text-align:left;">
Feb 04 1920 - Mar 06 1920
</td>
<td style="text-align:right;">
17
</td>
<td style="text-align:left;">
Dec 15 1932 - Mar 18 1933
</td>
<td style="text-align:right;">
9
</td>
<td style="text-align:left;">
Feb 11 1920 - Mar 08 1920
</td>
<td style="text-align:right;">
12
</td>
<td style="text-align:left;">
Dec 11 1928 - Jan 10 1929
</td>
<td style="text-align:right;">
1
</td>
</tr>
</tbody>
</table>

``` r
nhlWrapper("/franchise-goalie-records",8, NULL, "franchise-goalie-records" )
```

<table class="table" style="margin-left: auto; margin-right: auto;">
<caption>
franchise-goalie-records
</caption>
<thead>
<tr>
<th style="text-align:right;">
data.id
</th>
<th style="text-align:left;">
data.activePlayer
</th>
<th style="text-align:left;">
data.firstName
</th>
<th style="text-align:right;">
data.franchiseId
</th>
<th style="text-align:left;">
data.franchiseName
</th>
<th style="text-align:right;">
data.gameTypeId
</th>
<th style="text-align:right;">
data.gamesPlayed
</th>
<th style="text-align:left;">
data.lastName
</th>
<th style="text-align:right;">
data.losses
</th>
<th style="text-align:left;">
data.mostGoalsAgainstDates
</th>
<th style="text-align:right;">
data.mostGoalsAgainstOneGame
</th>
<th style="text-align:left;">
data.mostSavesDates
</th>
<th style="text-align:left;">
data.mostSavesOneGame
</th>
<th style="text-align:left;">
data.mostShotsAgainstDates
</th>
<th style="text-align:left;">
data.mostShotsAgainstOneGame
</th>
<th style="text-align:right;">
data.mostShutoutsOneSeason
</th>
<th style="text-align:left;">
data.mostShutoutsSeasonIds
</th>
<th style="text-align:right;">
data.mostWinsOneSeason
</th>
<th style="text-align:left;">
data.mostWinsSeasonIds
</th>
<th style="text-align:left;">
data.overtimeLosses
</th>
<th style="text-align:right;">
data.playerId
</th>
<th style="text-align:left;">
data.positionCode
</th>
<th style="text-align:right;">
data.rookieGamesPlayed
</th>
<th style="text-align:right;">
data.rookieShutouts
</th>
<th style="text-align:right;">
data.rookieWins
</th>
<th style="text-align:right;">
data.seasons
</th>
<th style="text-align:right;">
data.shutouts
</th>
<th style="text-align:right;">
data.ties
</th>
<th style="text-align:right;">
data.wins
</th>
<th style="text-align:right;">
total
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:right;">
552
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:left;">
Lorne
</td>
<td style="text-align:right;">
8
</td>
<td style="text-align:left;">
Brooklyn Americans
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
6
</td>
<td style="text-align:left;">
Chabot
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:left;">
1937-01-26
</td>
<td style="text-align:right;">
9
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:left;">
19361937
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:left;">
19361937
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:right;">
8449850
</td>
<td style="text-align:left;">
G
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
11
</td>
</tr>
<tr>
<td style="text-align:right;">
557
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:left;">
Alec
</td>
<td style="text-align:right;">
8
</td>
<td style="text-align:left;">
Brooklyn Americans
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:left;">
Connell
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:left;">
1934-03-15
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:left;">
19331934
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:left;">
19331934
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:right;">
8449856
</td>
<td style="text-align:left;">
G
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
11
</td>
</tr>
<tr>
<td style="text-align:right;">
560
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:left;">
Abbie
</td>
<td style="text-align:right;">
8
</td>
<td style="text-align:left;">
Brooklyn Americans
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:left;">
Cox
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:left;">
1933-11-12
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:left;">
19331934
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:left;">
19331934
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:right;">
8449858
</td>
<td style="text-align:left;">
G
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
11
</td>
</tr>
<tr>
<td style="text-align:right;">
578
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:left;">
Jake
</td>
<td style="text-align:right;">
8
</td>
<td style="text-align:left;">
Brooklyn Americans
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
105
</td>
<td style="text-align:left;">
Forbes
</td>
<td style="text-align:right;">
59
</td>
<td style="text-align:left;">
1928-03-18, 1926-02-18
</td>
<td style="text-align:right;">
7
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:right;">
8
</td>
<td style="text-align:left;">
19261927
</td>
<td style="text-align:right;">
17
</td>
<td style="text-align:left;">
19261927
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:right;">
8449918
</td>
<td style="text-align:left;">
G
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
7
</td>
<td style="text-align:right;">
12
</td>
<td style="text-align:right;">
10
</td>
<td style="text-align:right;">
36
</td>
<td style="text-align:right;">
11
</td>
</tr>
<tr>
<td style="text-align:right;">
593
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:left;">
Benny
</td>
<td style="text-align:right;">
8
</td>
<td style="text-align:left;">
Brooklyn Americans
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
12
</td>
<td style="text-align:left;">
Grant
</td>
<td style="text-align:right;">
7
</td>
<td style="text-align:left;">
1933-12-14, 1933-12-10, 1930-01-01, 1929-12-21, 1929-12-19
</td>
<td style="text-align:right;">
5
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:left;">
19331934
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:left;">
19291930
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:right;">
8449983
</td>
<td style="text-align:left;">
G
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
4
</td>
<td style="text-align:right;">
11
</td>
</tr>
<tr>
<td style="text-align:right;">
607
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:left;">
Percy
</td>
<td style="text-align:right;">
8
</td>
<td style="text-align:left;">
Brooklyn Americans
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:left;">
Jackson
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:left;">
1934-03-18
</td>
<td style="text-align:right;">
9
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:left;">
19331934
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:left;">
19331934
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:right;">
8450003
</td>
<td style="text-align:left;">
G
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
11
</td>
</tr>
</tbody>
</table>

``` r
#nhlWrapper("/franchise-skater-records",4, NULL, "franchise-skater-records" )
#nhlWrapper("/franchise-detail",2, NULL, "franchise-detail" )
#getNhlData(NULL, "Montreal Wanderers", "franchise")
#getNhlData(6, "Montreal Wanderers", NULL)
#getNhlData(NULL, NULL, NULL)

# *****************TESTING END***********************
```

## Exploratory Data Analysis (EDA)

Data from at least two endpoints (possibly combining them into one).
Here I used inner join function to join data from team stats and
Franchise dataset. Used select function to get only limited columns from
this joined data. I had to call api again since my wrapper function is
using kable function on dataset in order to test and provide details on
readme file and kable datatype can not be used to performed actions like
select, group\_by,…

``` r
#Team Stat data
res <- GET(paste0(team_stat_url,team_extend_url))
teams_stat <- fromJSON(rawToChar(res$content), flatten = TRUE)
teams_stat <- data.frame(teams_stat) %>% rename("data.id"="teams.franchiseId")

# Franchise data
res <- GET(paste0(base_url, "/", "franchise"))
franchise_join_data <- fromJSON(rawToChar(res$content), flatten = TRUE)
franchise_join_data <- data.frame(franchise_join_data)


#Fanchise and Team stat joint data
franchise_team_stat <- inner_join(franchise_join_data, teams_stat, by = "data.id")
franchise_team_stat <- franchise_team_stat %>% select("franchiseID"="data.id","teams.id", "teams.firstYearOfPlay", "teams.active","teams.venue.city","teams.venue.id","teams.division.id","teams.division.name", "teams.conference.id", "teams.conference.name" ) %>% na.omit()
head(franchise_team_stat)
```

    ##   franchiseID teams.id teams.firstYearOfPlay teams.active teams.venue.city
    ## 1           1        8                  1909         TRUE        MontrÃ©al
    ## 3           6        6                  1924         TRUE           Boston
    ## 4          10        3                  1926         TRUE         New York
    ## 5          11       16                  1926         TRUE          Chicago
    ## 6          12       17                  1926         TRUE          Detroit
    ## 7          14       26                  1967         TRUE      Los Angeles
    ##   teams.venue.id teams.division.id teams.division.name teams.conference.id
    ## 1           5028                28        Scotia North                   6
    ## 3           5085                25     MassMutual East                   6
    ## 4           5054                25     MassMutual East                   6
    ## 5           5092                26    Discover Central                   5
    ## 6           5145                26    Discover Central                   6
    ## 7           5081                27          Honda West                   5
    ##   teams.conference.name
    ## 1               Eastern
    ## 3               Eastern
    ## 4               Eastern
    ## 5               Western
    ## 6               Eastern
    ## 7               Western

### Create new variables

Used franchise-goalie-records table for further analysis. Created new
variables winRate and lossRate using mutate function. Mutate function
adds these predictor variables in the existing dataset.

``` r
WinLossRate <- function() {
  res <- GET(paste0(paste0(base_url, "/", "franchise-goalie-records",urlExtention,12)))
  goli_12_data <- fromJSON(rawToChar(res$content), flatten = TRUE) 
  goli_12_data <- goli_12_data %>% data.frame(goli_12_data) %>% select(data.activePlayer, data.firstName, data.franchiseName, data.gamesPlayed, data.lastName, data.gameTypeId, data.playerId, data.mostGoalsAgainstOneGame, data.ties, data.wins, data.losses ) %>% mutate(winRate= round(data.wins/data.gamesPlayed, 2) , lossRate= round(data.losses/data.gamesPlayed,2))
  return (goli_12_data)
}


goli_12_data <- WinLossRate ()
head(goli_12_data)
```

    ##   data.activePlayer data.firstName data.franchiseName data.gamesPlayed
    ## 1             FALSE          Allan  Detroit Red Wings                4
    ## 2             FALSE          Alain  Detroit Red Wings                3
    ## 3             FALSE            Joe  Detroit Red Wings               29
    ## 4             FALSE         Darren  Detroit Red Wings                3
    ## 5             FALSE            Bob  Detroit Red Wings               13
    ## 6             FALSE         Gilles  Detroit Red Wings               95
    ##   data.lastName data.gameTypeId data.playerId data.mostGoalsAgainstOneGame
    ## 1        Bester               2       8445458                            6
    ## 2      Chevrier               2       8446052                            5
    ## 3         Daley               2       8446290                            8
    ## 4         Eliot               2       8446637                            4
    ## 5       Essensa               2       8446719                            5
    ## 6       Gilbert               2       8447170                            8
    ##   data.ties data.wins data.losses winRate lossRate
    ## 1         0         0           3    0.00     0.75
    ## 2         0         0           2    0.00     0.67
    ## 3         5        11          10    0.38     0.34
    ## 4         1         0           0    0.00     0.00
    ## 5         2         4           7    0.31     0.54
    ## 6        16        21          48    0.22     0.51

### Contingency Tables

1.  Contingency table for mostGoalsAgainstOneGame and player’s lastName.
    This is two way contingency table. We used table() function to
    create contingency tables.
2.  Contingency tables created for games played and if player is active.
    This is two way contingency table. Observation is data is mostly
    available for Inactive Players. There are only 3 players active for
    the games played \#9, \#34 and \#105.
3.  Three way contingency table for goli’s lastname, most goals against
    one game and active player. It created 2 tables, first with inactive
    player and another one with active player. I see all the records in
    one table due to kable function.

``` r
kable( table(head(goli_12_data) %>% group_by(data.mostGoalsAgainstOneGame) %>% select(data.mostGoalsAgainstOneGame, data.lastName))) %>% kable_styling() 
```

<table class="table" style="margin-left: auto; margin-right: auto;">
<thead>
<tr>
<th style="text-align:left;">
</th>
<th style="text-align:right;">
Bester
</th>
<th style="text-align:right;">
Chevrier
</th>
<th style="text-align:right;">
Daley
</th>
<th style="text-align:right;">
Eliot
</th>
<th style="text-align:right;">
Essensa
</th>
<th style="text-align:right;">
Gilbert
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
4
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
5
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
6
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
8
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
</tr>
</tbody>
</table>

``` r
#Contingency table for #games played and if player is active
kable(table(goli_12_data$data.gamesPlayed, goli_12_data$data.activePlayer)) %>% kable_styling()
```

<table class="table" style="margin-left: auto; margin-right: auto;">
<thead>
<tr>
<th style="text-align:left;">
</th>
<th style="text-align:right;">
FALSE
</th>
<th style="text-align:right;">
TRUE
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
1
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
2
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
3
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
4
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
9
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
10
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
13
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
25
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
29
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
32
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
34
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
37
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
41
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
43
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
48
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
49
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
55
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
71
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
85
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
89
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
92
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
95
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
99
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
105
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
109
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
148
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
152
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
176
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
178
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
180
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
186
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
221
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
310
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
324
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
543
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
565
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
734
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
</tr>
</tbody>
</table>

``` r
#Contingency table for #player last name, most goals against one game and if player is active
kable(table(goli_12_data$data.lastName,goli_12_data$data.mostGoalsAgainstOneGame, goli_12_data$data.activePlayer)) %>% kable_styling()
```

<table class="table" style="margin-left: auto; margin-right: auto;">
<thead>
<tr>
<th style="text-align:left;">
Var1
</th>
<th style="text-align:left;">
Var2
</th>
<th style="text-align:left;">
Var3
</th>
<th style="text-align:right;">
Freq
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
Bassen
</td>
<td style="text-align:left;">
2
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Bernier
</td>
<td style="text-align:left;">
2
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Bester
</td>
<td style="text-align:left;">
2
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Bourque
</td>
<td style="text-align:left;">
2
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Chevrier
</td>
<td style="text-align:left;">
2
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Conklin
</td>
<td style="text-align:left;">
2
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Connell
</td>
<td style="text-align:left;">
2
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Cox
</td>
<td style="text-align:left;">
2
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Crozier
</td>
<td style="text-align:left;">
2
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Cude
</td>
<td style="text-align:left;">
2
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Daley
</td>
<td style="text-align:left;">
2
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
DeJordy
</td>
<td style="text-align:left;">
2
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Edwards
</td>
<td style="text-align:left;">
2
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Eliot
</td>
<td style="text-align:left;">
2
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Essensa
</td>
<td style="text-align:left;">
2
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Giacomin
</td>
<td style="text-align:left;">
2
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Gilbert
</td>
<td style="text-align:left;">
2
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Greiss
</td>
<td style="text-align:left;">
2
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Gustavsson
</td>
<td style="text-align:left;">
2
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Hall
</td>
<td style="text-align:left;">
2
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Hanlon
</td>
<td style="text-align:left;">
2
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Hasek
</td>
<td style="text-align:left;">
2
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Holmes
</td>
<td style="text-align:left;">
2
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Howard
</td>
<td style="text-align:left;">
2
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Ing
</td>
<td style="text-align:left;">
2
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Joseph
</td>
<td style="text-align:left;">
2
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Lamothe
</td>
<td style="text-align:left;">
2
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
Legace
</td>
<td style="text-align:left;">
2
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Low
</td>
<td style="text-align:left;">
2
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Lumley
</td>
<td style="text-align:left;">
2
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
MacDonald
</td>
<td style="text-align:left;">
2
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
McDuffe
</td>
<td style="text-align:left;">
2
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Millen
</td>
<td style="text-align:left;">
2
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Mio
</td>
<td style="text-align:left;">
2
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Moore
</td>
<td style="text-align:left;">
2
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Mowers
</td>
<td style="text-align:left;">
2
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Osgood
</td>
<td style="text-align:left;">
2
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Pickard
</td>
<td style="text-align:left;">
2
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Ranford
</td>
<td style="text-align:left;">
2
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Riendeau
</td>
<td style="text-align:left;">
2
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Roach
</td>
<td style="text-align:left;">
2
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Sauve
</td>
<td style="text-align:left;">
2
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Sawchuk
</td>
<td style="text-align:left;">
2
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Smith
</td>
<td style="text-align:left;">
2
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Thompson
</td>
<td style="text-align:left;">
2
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Vachon
</td>
<td style="text-align:left;">
2
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Vernon
</td>
<td style="text-align:left;">
2
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Wregget
</td>
<td style="text-align:left;">
2
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Bassen
</td>
<td style="text-align:left;">
3
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Bernier
</td>
<td style="text-align:left;">
3
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Bester
</td>
<td style="text-align:left;">
3
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Bourque
</td>
<td style="text-align:left;">
3
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
Chevrier
</td>
<td style="text-align:left;">
3
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Conklin
</td>
<td style="text-align:left;">
3
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Connell
</td>
<td style="text-align:left;">
3
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Cox
</td>
<td style="text-align:left;">
3
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Crozier
</td>
<td style="text-align:left;">
3
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Cude
</td>
<td style="text-align:left;">
3
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Daley
</td>
<td style="text-align:left;">
3
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
DeJordy
</td>
<td style="text-align:left;">
3
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Edwards
</td>
<td style="text-align:left;">
3
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Eliot
</td>
<td style="text-align:left;">
3
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Essensa
</td>
<td style="text-align:left;">
3
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Giacomin
</td>
<td style="text-align:left;">
3
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Gilbert
</td>
<td style="text-align:left;">
3
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Greiss
</td>
<td style="text-align:left;">
3
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Gustavsson
</td>
<td style="text-align:left;">
3
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Hall
</td>
<td style="text-align:left;">
3
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Hanlon
</td>
<td style="text-align:left;">
3
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Hasek
</td>
<td style="text-align:left;">
3
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Holmes
</td>
<td style="text-align:left;">
3
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Howard
</td>
<td style="text-align:left;">
3
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Ing
</td>
<td style="text-align:left;">
3
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Joseph
</td>
<td style="text-align:left;">
3
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Lamothe
</td>
<td style="text-align:left;">
3
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Legace
</td>
<td style="text-align:left;">
3
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Low
</td>
<td style="text-align:left;">
3
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Lumley
</td>
<td style="text-align:left;">
3
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
MacDonald
</td>
<td style="text-align:left;">
3
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
McDuffe
</td>
<td style="text-align:left;">
3
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Millen
</td>
<td style="text-align:left;">
3
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Mio
</td>
<td style="text-align:left;">
3
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Moore
</td>
<td style="text-align:left;">
3
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
Mowers
</td>
<td style="text-align:left;">
3
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Osgood
</td>
<td style="text-align:left;">
3
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Pickard
</td>
<td style="text-align:left;">
3
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Ranford
</td>
<td style="text-align:left;">
3
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
Riendeau
</td>
<td style="text-align:left;">
3
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Roach
</td>
<td style="text-align:left;">
3
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Sauve
</td>
<td style="text-align:left;">
3
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Sawchuk
</td>
<td style="text-align:left;">
3
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Smith
</td>
<td style="text-align:left;">
3
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Thompson
</td>
<td style="text-align:left;">
3
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Vachon
</td>
<td style="text-align:left;">
3
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Vernon
</td>
<td style="text-align:left;">
3
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Wregget
</td>
<td style="text-align:left;">
3
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Bassen
</td>
<td style="text-align:left;">
4
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Bernier
</td>
<td style="text-align:left;">
4
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Bester
</td>
<td style="text-align:left;">
4
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Bourque
</td>
<td style="text-align:left;">
4
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Chevrier
</td>
<td style="text-align:left;">
4
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Conklin
</td>
<td style="text-align:left;">
4
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Connell
</td>
<td style="text-align:left;">
4
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Cox
</td>
<td style="text-align:left;">
4
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
Crozier
</td>
<td style="text-align:left;">
4
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Cude
</td>
<td style="text-align:left;">
4
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Daley
</td>
<td style="text-align:left;">
4
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
DeJordy
</td>
<td style="text-align:left;">
4
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Edwards
</td>
<td style="text-align:left;">
4
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Eliot
</td>
<td style="text-align:left;">
4
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
Essensa
</td>
<td style="text-align:left;">
4
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Giacomin
</td>
<td style="text-align:left;">
4
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Gilbert
</td>
<td style="text-align:left;">
4
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Greiss
</td>
<td style="text-align:left;">
4
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Gustavsson
</td>
<td style="text-align:left;">
4
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Hall
</td>
<td style="text-align:left;">
4
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Hanlon
</td>
<td style="text-align:left;">
4
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Hasek
</td>
<td style="text-align:left;">
4
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Holmes
</td>
<td style="text-align:left;">
4
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Howard
</td>
<td style="text-align:left;">
4
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Ing
</td>
<td style="text-align:left;">
4
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Joseph
</td>
<td style="text-align:left;">
4
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Lamothe
</td>
<td style="text-align:left;">
4
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Legace
</td>
<td style="text-align:left;">
4
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Low
</td>
<td style="text-align:left;">
4
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Lumley
</td>
<td style="text-align:left;">
4
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
MacDonald
</td>
<td style="text-align:left;">
4
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
McDuffe
</td>
<td style="text-align:left;">
4
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Millen
</td>
<td style="text-align:left;">
4
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
Mio
</td>
<td style="text-align:left;">
4
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Moore
</td>
<td style="text-align:left;">
4
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Mowers
</td>
<td style="text-align:left;">
4
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Osgood
</td>
<td style="text-align:left;">
4
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Pickard
</td>
<td style="text-align:left;">
4
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Ranford
</td>
<td style="text-align:left;">
4
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Riendeau
</td>
<td style="text-align:left;">
4
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Roach
</td>
<td style="text-align:left;">
4
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Sauve
</td>
<td style="text-align:left;">
4
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Sawchuk
</td>
<td style="text-align:left;">
4
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Smith
</td>
<td style="text-align:left;">
4
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Thompson
</td>
<td style="text-align:left;">
4
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Vachon
</td>
<td style="text-align:left;">
4
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Vernon
</td>
<td style="text-align:left;">
4
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Wregget
</td>
<td style="text-align:left;">
4
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Bassen
</td>
<td style="text-align:left;">
5
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Bernier
</td>
<td style="text-align:left;">
5
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Bester
</td>
<td style="text-align:left;">
5
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Bourque
</td>
<td style="text-align:left;">
5
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Chevrier
</td>
<td style="text-align:left;">
5
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
Conklin
</td>
<td style="text-align:left;">
5
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Connell
</td>
<td style="text-align:left;">
5
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Cox
</td>
<td style="text-align:left;">
5
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Crozier
</td>
<td style="text-align:left;">
5
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Cude
</td>
<td style="text-align:left;">
5
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Daley
</td>
<td style="text-align:left;">
5
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
DeJordy
</td>
<td style="text-align:left;">
5
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Edwards
</td>
<td style="text-align:left;">
5
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Eliot
</td>
<td style="text-align:left;">
5
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Essensa
</td>
<td style="text-align:left;">
5
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
Giacomin
</td>
<td style="text-align:left;">
5
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Gilbert
</td>
<td style="text-align:left;">
5
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Greiss
</td>
<td style="text-align:left;">
5
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Gustavsson
</td>
<td style="text-align:left;">
5
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
Hall
</td>
<td style="text-align:left;">
5
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Hanlon
</td>
<td style="text-align:left;">
5
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Hasek
</td>
<td style="text-align:left;">
5
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Holmes
</td>
<td style="text-align:left;">
5
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Howard
</td>
<td style="text-align:left;">
5
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Ing
</td>
<td style="text-align:left;">
5
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Joseph
</td>
<td style="text-align:left;">
5
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Lamothe
</td>
<td style="text-align:left;">
5
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Legace
</td>
<td style="text-align:left;">
5
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Low
</td>
<td style="text-align:left;">
5
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Lumley
</td>
<td style="text-align:left;">
5
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
MacDonald
</td>
<td style="text-align:left;">
5
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
McDuffe
</td>
<td style="text-align:left;">
5
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Millen
</td>
<td style="text-align:left;">
5
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Mio
</td>
<td style="text-align:left;">
5
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Moore
</td>
<td style="text-align:left;">
5
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Mowers
</td>
<td style="text-align:left;">
5
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Osgood
</td>
<td style="text-align:left;">
5
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Pickard
</td>
<td style="text-align:left;">
5
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Ranford
</td>
<td style="text-align:left;">
5
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Riendeau
</td>
<td style="text-align:left;">
5
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Roach
</td>
<td style="text-align:left;">
5
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Sauve
</td>
<td style="text-align:left;">
5
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Sawchuk
</td>
<td style="text-align:left;">
5
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Smith
</td>
<td style="text-align:left;">
5
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Thompson
</td>
<td style="text-align:left;">
5
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Vachon
</td>
<td style="text-align:left;">
5
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Vernon
</td>
<td style="text-align:left;">
5
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Wregget
</td>
<td style="text-align:left;">
5
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Bassen
</td>
<td style="text-align:left;">
6
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Bernier
</td>
<td style="text-align:left;">
6
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Bester
</td>
<td style="text-align:left;">
6
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
Bourque
</td>
<td style="text-align:left;">
6
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Chevrier
</td>
<td style="text-align:left;">
6
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Conklin
</td>
<td style="text-align:left;">
6
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Connell
</td>
<td style="text-align:left;">
6
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Cox
</td>
<td style="text-align:left;">
6
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Crozier
</td>
<td style="text-align:left;">
6
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Cude
</td>
<td style="text-align:left;">
6
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
Daley
</td>
<td style="text-align:left;">
6
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
DeJordy
</td>
<td style="text-align:left;">
6
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Edwards
</td>
<td style="text-align:left;">
6
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Eliot
</td>
<td style="text-align:left;">
6
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Essensa
</td>
<td style="text-align:left;">
6
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Giacomin
</td>
<td style="text-align:left;">
6
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Gilbert
</td>
<td style="text-align:left;">
6
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Greiss
</td>
<td style="text-align:left;">
6
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Gustavsson
</td>
<td style="text-align:left;">
6
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Hall
</td>
<td style="text-align:left;">
6
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Hanlon
</td>
<td style="text-align:left;">
6
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Hasek
</td>
<td style="text-align:left;">
6
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Holmes
</td>
<td style="text-align:left;">
6
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Howard
</td>
<td style="text-align:left;">
6
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Ing
</td>
<td style="text-align:left;">
6
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Joseph
</td>
<td style="text-align:left;">
6
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
Lamothe
</td>
<td style="text-align:left;">
6
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Legace
</td>
<td style="text-align:left;">
6
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
Low
</td>
<td style="text-align:left;">
6
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Lumley
</td>
<td style="text-align:left;">
6
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
MacDonald
</td>
<td style="text-align:left;">
6
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
McDuffe
</td>
<td style="text-align:left;">
6
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Millen
</td>
<td style="text-align:left;">
6
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Mio
</td>
<td style="text-align:left;">
6
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Moore
</td>
<td style="text-align:left;">
6
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Mowers
</td>
<td style="text-align:left;">
6
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Osgood
</td>
<td style="text-align:left;">
6
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Pickard
</td>
<td style="text-align:left;">
6
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Ranford
</td>
<td style="text-align:left;">
6
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Riendeau
</td>
<td style="text-align:left;">
6
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Roach
</td>
<td style="text-align:left;">
6
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Sauve
</td>
<td style="text-align:left;">
6
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Sawchuk
</td>
<td style="text-align:left;">
6
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Smith
</td>
<td style="text-align:left;">
6
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Thompson
</td>
<td style="text-align:left;">
6
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Vachon
</td>
<td style="text-align:left;">
6
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Vernon
</td>
<td style="text-align:left;">
6
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
Wregget
</td>
<td style="text-align:left;">
6
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
Bassen
</td>
<td style="text-align:left;">
7
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Bernier
</td>
<td style="text-align:left;">
7
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Bester
</td>
<td style="text-align:left;">
7
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Bourque
</td>
<td style="text-align:left;">
7
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Chevrier
</td>
<td style="text-align:left;">
7
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Conklin
</td>
<td style="text-align:left;">
7
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
Connell
</td>
<td style="text-align:left;">
7
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
Cox
</td>
<td style="text-align:left;">
7
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Crozier
</td>
<td style="text-align:left;">
7
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Cude
</td>
<td style="text-align:left;">
7
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Daley
</td>
<td style="text-align:left;">
7
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
DeJordy
</td>
<td style="text-align:left;">
7
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Edwards
</td>
<td style="text-align:left;">
7
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Eliot
</td>
<td style="text-align:left;">
7
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Essensa
</td>
<td style="text-align:left;">
7
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Giacomin
</td>
<td style="text-align:left;">
7
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Gilbert
</td>
<td style="text-align:left;">
7
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Greiss
</td>
<td style="text-align:left;">
7
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Gustavsson
</td>
<td style="text-align:left;">
7
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Hall
</td>
<td style="text-align:left;">
7
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
Hanlon
</td>
<td style="text-align:left;">
7
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Hasek
</td>
<td style="text-align:left;">
7
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Holmes
</td>
<td style="text-align:left;">
7
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
Howard
</td>
<td style="text-align:left;">
7
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
Ing
</td>
<td style="text-align:left;">
7
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Joseph
</td>
<td style="text-align:left;">
7
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Lamothe
</td>
<td style="text-align:left;">
7
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Legace
</td>
<td style="text-align:left;">
7
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Low
</td>
<td style="text-align:left;">
7
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Lumley
</td>
<td style="text-align:left;">
7
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
MacDonald
</td>
<td style="text-align:left;">
7
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
McDuffe
</td>
<td style="text-align:left;">
7
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Millen
</td>
<td style="text-align:left;">
7
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Mio
</td>
<td style="text-align:left;">
7
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Moore
</td>
<td style="text-align:left;">
7
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Mowers
</td>
<td style="text-align:left;">
7
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Osgood
</td>
<td style="text-align:left;">
7
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
Pickard
</td>
<td style="text-align:left;">
7
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Ranford
</td>
<td style="text-align:left;">
7
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Riendeau
</td>
<td style="text-align:left;">
7
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
Roach
</td>
<td style="text-align:left;">
7
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Sauve
</td>
<td style="text-align:left;">
7
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Sawchuk
</td>
<td style="text-align:left;">
7
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Smith
</td>
<td style="text-align:left;">
7
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
Thompson
</td>
<td style="text-align:left;">
7
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Vachon
</td>
<td style="text-align:left;">
7
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Vernon
</td>
<td style="text-align:left;">
7
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Wregget
</td>
<td style="text-align:left;">
7
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Bassen
</td>
<td style="text-align:left;">
8
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
Bernier
</td>
<td style="text-align:left;">
8
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Bester
</td>
<td style="text-align:left;">
8
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Bourque
</td>
<td style="text-align:left;">
8
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Chevrier
</td>
<td style="text-align:left;">
8
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Conklin
</td>
<td style="text-align:left;">
8
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Connell
</td>
<td style="text-align:left;">
8
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Cox
</td>
<td style="text-align:left;">
8
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Crozier
</td>
<td style="text-align:left;">
8
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Cude
</td>
<td style="text-align:left;">
8
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Daley
</td>
<td style="text-align:left;">
8
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
DeJordy
</td>
<td style="text-align:left;">
8
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
Edwards
</td>
<td style="text-align:left;">
8
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Eliot
</td>
<td style="text-align:left;">
8
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Essensa
</td>
<td style="text-align:left;">
8
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Giacomin
</td>
<td style="text-align:left;">
8
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Gilbert
</td>
<td style="text-align:left;">
8
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
Greiss
</td>
<td style="text-align:left;">
8
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Gustavsson
</td>
<td style="text-align:left;">
8
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Hall
</td>
<td style="text-align:left;">
8
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Hanlon
</td>
<td style="text-align:left;">
8
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Hasek
</td>
<td style="text-align:left;">
8
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
Holmes
</td>
<td style="text-align:left;">
8
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Howard
</td>
<td style="text-align:left;">
8
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Ing
</td>
<td style="text-align:left;">
8
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
Joseph
</td>
<td style="text-align:left;">
8
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Lamothe
</td>
<td style="text-align:left;">
8
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Legace
</td>
<td style="text-align:left;">
8
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Low
</td>
<td style="text-align:left;">
8
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Lumley
</td>
<td style="text-align:left;">
8
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
MacDonald
</td>
<td style="text-align:left;">
8
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
McDuffe
</td>
<td style="text-align:left;">
8
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
Millen
</td>
<td style="text-align:left;">
8
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Mio
</td>
<td style="text-align:left;">
8
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
Moore
</td>
<td style="text-align:left;">
8
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Mowers
</td>
<td style="text-align:left;">
8
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Osgood
</td>
<td style="text-align:left;">
8
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Pickard
</td>
<td style="text-align:left;">
8
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Ranford
</td>
<td style="text-align:left;">
8
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Riendeau
</td>
<td style="text-align:left;">
8
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Roach
</td>
<td style="text-align:left;">
8
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
Sauve
</td>
<td style="text-align:left;">
8
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
Sawchuk
</td>
<td style="text-align:left;">
8
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Smith
</td>
<td style="text-align:left;">
8
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Thompson
</td>
<td style="text-align:left;">
8
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Vachon
</td>
<td style="text-align:left;">
8
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
Vernon
</td>
<td style="text-align:left;">
8
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Wregget
</td>
<td style="text-align:left;">
8
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Bassen
</td>
<td style="text-align:left;">
9
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Bernier
</td>
<td style="text-align:left;">
9
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Bester
</td>
<td style="text-align:left;">
9
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Bourque
</td>
<td style="text-align:left;">
9
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Chevrier
</td>
<td style="text-align:left;">
9
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Conklin
</td>
<td style="text-align:left;">
9
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Connell
</td>
<td style="text-align:left;">
9
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Cox
</td>
<td style="text-align:left;">
9
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Crozier
</td>
<td style="text-align:left;">
9
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Cude
</td>
<td style="text-align:left;">
9
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Daley
</td>
<td style="text-align:left;">
9
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
DeJordy
</td>
<td style="text-align:left;">
9
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Edwards
</td>
<td style="text-align:left;">
9
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Eliot
</td>
<td style="text-align:left;">
9
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Essensa
</td>
<td style="text-align:left;">
9
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Giacomin
</td>
<td style="text-align:left;">
9
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
Gilbert
</td>
<td style="text-align:left;">
9
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Greiss
</td>
<td style="text-align:left;">
9
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Gustavsson
</td>
<td style="text-align:left;">
9
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Hall
</td>
<td style="text-align:left;">
9
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Hanlon
</td>
<td style="text-align:left;">
9
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Hasek
</td>
<td style="text-align:left;">
9
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Holmes
</td>
<td style="text-align:left;">
9
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Howard
</td>
<td style="text-align:left;">
9
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Ing
</td>
<td style="text-align:left;">
9
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Joseph
</td>
<td style="text-align:left;">
9
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Lamothe
</td>
<td style="text-align:left;">
9
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Legace
</td>
<td style="text-align:left;">
9
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Low
</td>
<td style="text-align:left;">
9
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
Lumley
</td>
<td style="text-align:left;">
9
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
MacDonald
</td>
<td style="text-align:left;">
9
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
McDuffe
</td>
<td style="text-align:left;">
9
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Millen
</td>
<td style="text-align:left;">
9
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Mio
</td>
<td style="text-align:left;">
9
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Moore
</td>
<td style="text-align:left;">
9
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Mowers
</td>
<td style="text-align:left;">
9
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Osgood
</td>
<td style="text-align:left;">
9
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Pickard
</td>
<td style="text-align:left;">
9
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Ranford
</td>
<td style="text-align:left;">
9
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Riendeau
</td>
<td style="text-align:left;">
9
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Roach
</td>
<td style="text-align:left;">
9
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Sauve
</td>
<td style="text-align:left;">
9
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Sawchuk
</td>
<td style="text-align:left;">
9
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Smith
</td>
<td style="text-align:left;">
9
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
Thompson
</td>
<td style="text-align:left;">
9
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Vachon
</td>
<td style="text-align:left;">
9
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Vernon
</td>
<td style="text-align:left;">
9
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Wregget
</td>
<td style="text-align:left;">
9
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Bassen
</td>
<td style="text-align:left;">
10
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Bernier
</td>
<td style="text-align:left;">
10
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Bester
</td>
<td style="text-align:left;">
10
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Bourque
</td>
<td style="text-align:left;">
10
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Chevrier
</td>
<td style="text-align:left;">
10
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Conklin
</td>
<td style="text-align:left;">
10
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Connell
</td>
<td style="text-align:left;">
10
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Cox
</td>
<td style="text-align:left;">
10
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Crozier
</td>
<td style="text-align:left;">
10
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
Cude
</td>
<td style="text-align:left;">
10
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Daley
</td>
<td style="text-align:left;">
10
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
DeJordy
</td>
<td style="text-align:left;">
10
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Edwards
</td>
<td style="text-align:left;">
10
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Eliot
</td>
<td style="text-align:left;">
10
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Essensa
</td>
<td style="text-align:left;">
10
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Giacomin
</td>
<td style="text-align:left;">
10
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Gilbert
</td>
<td style="text-align:left;">
10
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Greiss
</td>
<td style="text-align:left;">
10
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Gustavsson
</td>
<td style="text-align:left;">
10
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Hall
</td>
<td style="text-align:left;">
10
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Hanlon
</td>
<td style="text-align:left;">
10
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
Hasek
</td>
<td style="text-align:left;">
10
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Holmes
</td>
<td style="text-align:left;">
10
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Howard
</td>
<td style="text-align:left;">
10
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Ing
</td>
<td style="text-align:left;">
10
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Joseph
</td>
<td style="text-align:left;">
10
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Lamothe
</td>
<td style="text-align:left;">
10
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Legace
</td>
<td style="text-align:left;">
10
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Low
</td>
<td style="text-align:left;">
10
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Lumley
</td>
<td style="text-align:left;">
10
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
MacDonald
</td>
<td style="text-align:left;">
10
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
McDuffe
</td>
<td style="text-align:left;">
10
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Millen
</td>
<td style="text-align:left;">
10
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Mio
</td>
<td style="text-align:left;">
10
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Moore
</td>
<td style="text-align:left;">
10
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Mowers
</td>
<td style="text-align:left;">
10
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Osgood
</td>
<td style="text-align:left;">
10
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Pickard
</td>
<td style="text-align:left;">
10
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Ranford
</td>
<td style="text-align:left;">
10
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Riendeau
</td>
<td style="text-align:left;">
10
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Roach
</td>
<td style="text-align:left;">
10
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Sauve
</td>
<td style="text-align:left;">
10
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Sawchuk
</td>
<td style="text-align:left;">
10
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
Smith
</td>
<td style="text-align:left;">
10
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Thompson
</td>
<td style="text-align:left;">
10
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
Vachon
</td>
<td style="text-align:left;">
10
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Vernon
</td>
<td style="text-align:left;">
10
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Wregget
</td>
<td style="text-align:left;">
10
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Bassen
</td>
<td style="text-align:left;">
11
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Bernier
</td>
<td style="text-align:left;">
11
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Bester
</td>
<td style="text-align:left;">
11
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Bourque
</td>
<td style="text-align:left;">
11
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Chevrier
</td>
<td style="text-align:left;">
11
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Conklin
</td>
<td style="text-align:left;">
11
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Connell
</td>
<td style="text-align:left;">
11
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Cox
</td>
<td style="text-align:left;">
11
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Crozier
</td>
<td style="text-align:left;">
11
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Cude
</td>
<td style="text-align:left;">
11
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Daley
</td>
<td style="text-align:left;">
11
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
DeJordy
</td>
<td style="text-align:left;">
11
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Edwards
</td>
<td style="text-align:left;">
11
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
Eliot
</td>
<td style="text-align:left;">
11
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Essensa
</td>
<td style="text-align:left;">
11
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Giacomin
</td>
<td style="text-align:left;">
11
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Gilbert
</td>
<td style="text-align:left;">
11
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Greiss
</td>
<td style="text-align:left;">
11
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Gustavsson
</td>
<td style="text-align:left;">
11
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Hall
</td>
<td style="text-align:left;">
11
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Hanlon
</td>
<td style="text-align:left;">
11
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Hasek
</td>
<td style="text-align:left;">
11
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Holmes
</td>
<td style="text-align:left;">
11
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Howard
</td>
<td style="text-align:left;">
11
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Ing
</td>
<td style="text-align:left;">
11
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Joseph
</td>
<td style="text-align:left;">
11
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Lamothe
</td>
<td style="text-align:left;">
11
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Legace
</td>
<td style="text-align:left;">
11
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Low
</td>
<td style="text-align:left;">
11
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Lumley
</td>
<td style="text-align:left;">
11
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
MacDonald
</td>
<td style="text-align:left;">
11
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
McDuffe
</td>
<td style="text-align:left;">
11
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Millen
</td>
<td style="text-align:left;">
11
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Mio
</td>
<td style="text-align:left;">
11
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Moore
</td>
<td style="text-align:left;">
11
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Mowers
</td>
<td style="text-align:left;">
11
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
Osgood
</td>
<td style="text-align:left;">
11
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Pickard
</td>
<td style="text-align:left;">
11
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Ranford
</td>
<td style="text-align:left;">
11
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Riendeau
</td>
<td style="text-align:left;">
11
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Roach
</td>
<td style="text-align:left;">
11
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Sauve
</td>
<td style="text-align:left;">
11
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Sawchuk
</td>
<td style="text-align:left;">
11
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Smith
</td>
<td style="text-align:left;">
11
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Thompson
</td>
<td style="text-align:left;">
11
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Vachon
</td>
<td style="text-align:left;">
11
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Vernon
</td>
<td style="text-align:left;">
11
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Wregget
</td>
<td style="text-align:left;">
11
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Bassen
</td>
<td style="text-align:left;">
2
</td>
<td style="text-align:left;">
TRUE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Bernier
</td>
<td style="text-align:left;">
2
</td>
<td style="text-align:left;">
TRUE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Bester
</td>
<td style="text-align:left;">
2
</td>
<td style="text-align:left;">
TRUE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Bourque
</td>
<td style="text-align:left;">
2
</td>
<td style="text-align:left;">
TRUE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Chevrier
</td>
<td style="text-align:left;">
2
</td>
<td style="text-align:left;">
TRUE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Conklin
</td>
<td style="text-align:left;">
2
</td>
<td style="text-align:left;">
TRUE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Connell
</td>
<td style="text-align:left;">
2
</td>
<td style="text-align:left;">
TRUE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Cox
</td>
<td style="text-align:left;">
2
</td>
<td style="text-align:left;">
TRUE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Crozier
</td>
<td style="text-align:left;">
2
</td>
<td style="text-align:left;">
TRUE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Cude
</td>
<td style="text-align:left;">
2
</td>
<td style="text-align:left;">
TRUE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Daley
</td>
<td style="text-align:left;">
2
</td>
<td style="text-align:left;">
TRUE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
DeJordy
</td>
<td style="text-align:left;">
2
</td>
<td style="text-align:left;">
TRUE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Edwards
</td>
<td style="text-align:left;">
2
</td>
<td style="text-align:left;">
TRUE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Eliot
</td>
<td style="text-align:left;">
2
</td>
<td style="text-align:left;">
TRUE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Essensa
</td>
<td style="text-align:left;">
2
</td>
<td style="text-align:left;">
TRUE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Giacomin
</td>
<td style="text-align:left;">
2
</td>
<td style="text-align:left;">
TRUE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Gilbert
</td>
<td style="text-align:left;">
2
</td>
<td style="text-align:left;">
TRUE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Greiss
</td>
<td style="text-align:left;">
2
</td>
<td style="text-align:left;">
TRUE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Gustavsson
</td>
<td style="text-align:left;">
2
</td>
<td style="text-align:left;">
TRUE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Hall
</td>
<td style="text-align:left;">
2
</td>
<td style="text-align:left;">
TRUE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Hanlon
</td>
<td style="text-align:left;">
2
</td>
<td style="text-align:left;">
TRUE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Hasek
</td>
<td style="text-align:left;">
2
</td>
<td style="text-align:left;">
TRUE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Holmes
</td>
<td style="text-align:left;">
2
</td>
<td style="text-align:left;">
TRUE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Howard
</td>
<td style="text-align:left;">
2
</td>
<td style="text-align:left;">
TRUE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Ing
</td>
<td style="text-align:left;">
2
</td>
<td style="text-align:left;">
TRUE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Joseph
</td>
<td style="text-align:left;">
2
</td>
<td style="text-align:left;">
TRUE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Lamothe
</td>
<td style="text-align:left;">
2
</td>
<td style="text-align:left;">
TRUE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Legace
</td>
<td style="text-align:left;">
2
</td>
<td style="text-align:left;">
TRUE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Low
</td>
<td style="text-align:left;">
2
</td>
<td style="text-align:left;">
TRUE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Lumley
</td>
<td style="text-align:left;">
2
</td>
<td style="text-align:left;">
TRUE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
MacDonald
</td>
<td style="text-align:left;">
2
</td>
<td style="text-align:left;">
TRUE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
McDuffe
</td>
<td style="text-align:left;">
2
</td>
<td style="text-align:left;">
TRUE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Millen
</td>
<td style="text-align:left;">
2
</td>
<td style="text-align:left;">
TRUE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Mio
</td>
<td style="text-align:left;">
2
</td>
<td style="text-align:left;">
TRUE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Moore
</td>
<td style="text-align:left;">
2
</td>
<td style="text-align:left;">
TRUE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Mowers
</td>
<td style="text-align:left;">
2
</td>
<td style="text-align:left;">
TRUE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Osgood
</td>
<td style="text-align:left;">
2
</td>
<td style="text-align:left;">
TRUE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Pickard
</td>
<td style="text-align:left;">
2
</td>
<td style="text-align:left;">
TRUE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Ranford
</td>
<td style="text-align:left;">
2
</td>
<td style="text-align:left;">
TRUE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Riendeau
</td>
<td style="text-align:left;">
2
</td>
<td style="text-align:left;">
TRUE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Roach
</td>
<td style="text-align:left;">
2
</td>
<td style="text-align:left;">
TRUE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Sauve
</td>
<td style="text-align:left;">
2
</td>
<td style="text-align:left;">
TRUE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Sawchuk
</td>
<td style="text-align:left;">
2
</td>
<td style="text-align:left;">
TRUE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Smith
</td>
<td style="text-align:left;">
2
</td>
<td style="text-align:left;">
TRUE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Thompson
</td>
<td style="text-align:left;">
2
</td>
<td style="text-align:left;">
TRUE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Vachon
</td>
<td style="text-align:left;">
2
</td>
<td style="text-align:left;">
TRUE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Vernon
</td>
<td style="text-align:left;">
2
</td>
<td style="text-align:left;">
TRUE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Wregget
</td>
<td style="text-align:left;">
2
</td>
<td style="text-align:left;">
TRUE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Bassen
</td>
<td style="text-align:left;">
3
</td>
<td style="text-align:left;">
TRUE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Bernier
</td>
<td style="text-align:left;">
3
</td>
<td style="text-align:left;">
TRUE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Bester
</td>
<td style="text-align:left;">
3
</td>
<td style="text-align:left;">
TRUE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Bourque
</td>
<td style="text-align:left;">
3
</td>
<td style="text-align:left;">
TRUE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Chevrier
</td>
<td style="text-align:left;">
3
</td>
<td style="text-align:left;">
TRUE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Conklin
</td>
<td style="text-align:left;">
3
</td>
<td style="text-align:left;">
TRUE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Connell
</td>
<td style="text-align:left;">
3
</td>
<td style="text-align:left;">
TRUE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Cox
</td>
<td style="text-align:left;">
3
</td>
<td style="text-align:left;">
TRUE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Crozier
</td>
<td style="text-align:left;">
3
</td>
<td style="text-align:left;">
TRUE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Cude
</td>
<td style="text-align:left;">
3
</td>
<td style="text-align:left;">
TRUE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Daley
</td>
<td style="text-align:left;">
3
</td>
<td style="text-align:left;">
TRUE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
DeJordy
</td>
<td style="text-align:left;">
3
</td>
<td style="text-align:left;">
TRUE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Edwards
</td>
<td style="text-align:left;">
3
</td>
<td style="text-align:left;">
TRUE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Eliot
</td>
<td style="text-align:left;">
3
</td>
<td style="text-align:left;">
TRUE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Essensa
</td>
<td style="text-align:left;">
3
</td>
<td style="text-align:left;">
TRUE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Giacomin
</td>
<td style="text-align:left;">
3
</td>
<td style="text-align:left;">
TRUE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Gilbert
</td>
<td style="text-align:left;">
3
</td>
<td style="text-align:left;">
TRUE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Greiss
</td>
<td style="text-align:left;">
3
</td>
<td style="text-align:left;">
TRUE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Gustavsson
</td>
<td style="text-align:left;">
3
</td>
<td style="text-align:left;">
TRUE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Hall
</td>
<td style="text-align:left;">
3
</td>
<td style="text-align:left;">
TRUE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Hanlon
</td>
<td style="text-align:left;">
3
</td>
<td style="text-align:left;">
TRUE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Hasek
</td>
<td style="text-align:left;">
3
</td>
<td style="text-align:left;">
TRUE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Holmes
</td>
<td style="text-align:left;">
3
</td>
<td style="text-align:left;">
TRUE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Howard
</td>
<td style="text-align:left;">
3
</td>
<td style="text-align:left;">
TRUE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Ing
</td>
<td style="text-align:left;">
3
</td>
<td style="text-align:left;">
TRUE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Joseph
</td>
<td style="text-align:left;">
3
</td>
<td style="text-align:left;">
TRUE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Lamothe
</td>
<td style="text-align:left;">
3
</td>
<td style="text-align:left;">
TRUE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Legace
</td>
<td style="text-align:left;">
3
</td>
<td style="text-align:left;">
TRUE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Low
</td>
<td style="text-align:left;">
3
</td>
<td style="text-align:left;">
TRUE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Lumley
</td>
<td style="text-align:left;">
3
</td>
<td style="text-align:left;">
TRUE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
MacDonald
</td>
<td style="text-align:left;">
3
</td>
<td style="text-align:left;">
TRUE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
McDuffe
</td>
<td style="text-align:left;">
3
</td>
<td style="text-align:left;">
TRUE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Millen
</td>
<td style="text-align:left;">
3
</td>
<td style="text-align:left;">
TRUE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Mio
</td>
<td style="text-align:left;">
3
</td>
<td style="text-align:left;">
TRUE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Moore
</td>
<td style="text-align:left;">
3
</td>
<td style="text-align:left;">
TRUE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Mowers
</td>
<td style="text-align:left;">
3
</td>
<td style="text-align:left;">
TRUE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Osgood
</td>
<td style="text-align:left;">
3
</td>
<td style="text-align:left;">
TRUE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Pickard
</td>
<td style="text-align:left;">
3
</td>
<td style="text-align:left;">
TRUE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Ranford
</td>
<td style="text-align:left;">
3
</td>
<td style="text-align:left;">
TRUE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Riendeau
</td>
<td style="text-align:left;">
3
</td>
<td style="text-align:left;">
TRUE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Roach
</td>
<td style="text-align:left;">
3
</td>
<td style="text-align:left;">
TRUE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Sauve
</td>
<td style="text-align:left;">
3
</td>
<td style="text-align:left;">
TRUE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Sawchuk
</td>
<td style="text-align:left;">
3
</td>
<td style="text-align:left;">
TRUE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Smith
</td>
<td style="text-align:left;">
3
</td>
<td style="text-align:left;">
TRUE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Thompson
</td>
<td style="text-align:left;">
3
</td>
<td style="text-align:left;">
TRUE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Vachon
</td>
<td style="text-align:left;">
3
</td>
<td style="text-align:left;">
TRUE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Vernon
</td>
<td style="text-align:left;">
3
</td>
<td style="text-align:left;">
TRUE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Wregget
</td>
<td style="text-align:left;">
3
</td>
<td style="text-align:left;">
TRUE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Bassen
</td>
<td style="text-align:left;">
4
</td>
<td style="text-align:left;">
TRUE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Bernier
</td>
<td style="text-align:left;">
4
</td>
<td style="text-align:left;">
TRUE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Bester
</td>
<td style="text-align:left;">
4
</td>
<td style="text-align:left;">
TRUE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Bourque
</td>
<td style="text-align:left;">
4
</td>
<td style="text-align:left;">
TRUE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Chevrier
</td>
<td style="text-align:left;">
4
</td>
<td style="text-align:left;">
TRUE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Conklin
</td>
<td style="text-align:left;">
4
</td>
<td style="text-align:left;">
TRUE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Connell
</td>
<td style="text-align:left;">
4
</td>
<td style="text-align:left;">
TRUE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Cox
</td>
<td style="text-align:left;">
4
</td>
<td style="text-align:left;">
TRUE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Crozier
</td>
<td style="text-align:left;">
4
</td>
<td style="text-align:left;">
TRUE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Cude
</td>
<td style="text-align:left;">
4
</td>
<td style="text-align:left;">
TRUE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Daley
</td>
<td style="text-align:left;">
4
</td>
<td style="text-align:left;">
TRUE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
DeJordy
</td>
<td style="text-align:left;">
4
</td>
<td style="text-align:left;">
TRUE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Edwards
</td>
<td style="text-align:left;">
4
</td>
<td style="text-align:left;">
TRUE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Eliot
</td>
<td style="text-align:left;">
4
</td>
<td style="text-align:left;">
TRUE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Essensa
</td>
<td style="text-align:left;">
4
</td>
<td style="text-align:left;">
TRUE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Giacomin
</td>
<td style="text-align:left;">
4
</td>
<td style="text-align:left;">
TRUE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Gilbert
</td>
<td style="text-align:left;">
4
</td>
<td style="text-align:left;">
TRUE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Greiss
</td>
<td style="text-align:left;">
4
</td>
<td style="text-align:left;">
TRUE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Gustavsson
</td>
<td style="text-align:left;">
4
</td>
<td style="text-align:left;">
TRUE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Hall
</td>
<td style="text-align:left;">
4
</td>
<td style="text-align:left;">
TRUE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Hanlon
</td>
<td style="text-align:left;">
4
</td>
<td style="text-align:left;">
TRUE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Hasek
</td>
<td style="text-align:left;">
4
</td>
<td style="text-align:left;">
TRUE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Holmes
</td>
<td style="text-align:left;">
4
</td>
<td style="text-align:left;">
TRUE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Howard
</td>
<td style="text-align:left;">
4
</td>
<td style="text-align:left;">
TRUE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Ing
</td>
<td style="text-align:left;">
4
</td>
<td style="text-align:left;">
TRUE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Joseph
</td>
<td style="text-align:left;">
4
</td>
<td style="text-align:left;">
TRUE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Lamothe
</td>
<td style="text-align:left;">
4
</td>
<td style="text-align:left;">
TRUE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Legace
</td>
<td style="text-align:left;">
4
</td>
<td style="text-align:left;">
TRUE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Low
</td>
<td style="text-align:left;">
4
</td>
<td style="text-align:left;">
TRUE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Lumley
</td>
<td style="text-align:left;">
4
</td>
<td style="text-align:left;">
TRUE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
MacDonald
</td>
<td style="text-align:left;">
4
</td>
<td style="text-align:left;">
TRUE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
McDuffe
</td>
<td style="text-align:left;">
4
</td>
<td style="text-align:left;">
TRUE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Millen
</td>
<td style="text-align:left;">
4
</td>
<td style="text-align:left;">
TRUE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Mio
</td>
<td style="text-align:left;">
4
</td>
<td style="text-align:left;">
TRUE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Moore
</td>
<td style="text-align:left;">
4
</td>
<td style="text-align:left;">
TRUE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Mowers
</td>
<td style="text-align:left;">
4
</td>
<td style="text-align:left;">
TRUE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Osgood
</td>
<td style="text-align:left;">
4
</td>
<td style="text-align:left;">
TRUE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Pickard
</td>
<td style="text-align:left;">
4
</td>
<td style="text-align:left;">
TRUE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Ranford
</td>
<td style="text-align:left;">
4
</td>
<td style="text-align:left;">
TRUE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Riendeau
</td>
<td style="text-align:left;">
4
</td>
<td style="text-align:left;">
TRUE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Roach
</td>
<td style="text-align:left;">
4
</td>
<td style="text-align:left;">
TRUE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Sauve
</td>
<td style="text-align:left;">
4
</td>
<td style="text-align:left;">
TRUE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Sawchuk
</td>
<td style="text-align:left;">
4
</td>
<td style="text-align:left;">
TRUE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Smith
</td>
<td style="text-align:left;">
4
</td>
<td style="text-align:left;">
TRUE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Thompson
</td>
<td style="text-align:left;">
4
</td>
<td style="text-align:left;">
TRUE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Vachon
</td>
<td style="text-align:left;">
4
</td>
<td style="text-align:left;">
TRUE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Vernon
</td>
<td style="text-align:left;">
4
</td>
<td style="text-align:left;">
TRUE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Wregget
</td>
<td style="text-align:left;">
4
</td>
<td style="text-align:left;">
TRUE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Bassen
</td>
<td style="text-align:left;">
5
</td>
<td style="text-align:left;">
TRUE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Bernier
</td>
<td style="text-align:left;">
5
</td>
<td style="text-align:left;">
TRUE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Bester
</td>
<td style="text-align:left;">
5
</td>
<td style="text-align:left;">
TRUE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Bourque
</td>
<td style="text-align:left;">
5
</td>
<td style="text-align:left;">
TRUE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Chevrier
</td>
<td style="text-align:left;">
5
</td>
<td style="text-align:left;">
TRUE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Conklin
</td>
<td style="text-align:left;">
5
</td>
<td style="text-align:left;">
TRUE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Connell
</td>
<td style="text-align:left;">
5
</td>
<td style="text-align:left;">
TRUE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Cox
</td>
<td style="text-align:left;">
5
</td>
<td style="text-align:left;">
TRUE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Crozier
</td>
<td style="text-align:left;">
5
</td>
<td style="text-align:left;">
TRUE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Cude
</td>
<td style="text-align:left;">
5
</td>
<td style="text-align:left;">
TRUE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Daley
</td>
<td style="text-align:left;">
5
</td>
<td style="text-align:left;">
TRUE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
DeJordy
</td>
<td style="text-align:left;">
5
</td>
<td style="text-align:left;">
TRUE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Edwards
</td>
<td style="text-align:left;">
5
</td>
<td style="text-align:left;">
TRUE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Eliot
</td>
<td style="text-align:left;">
5
</td>
<td style="text-align:left;">
TRUE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Essensa
</td>
<td style="text-align:left;">
5
</td>
<td style="text-align:left;">
TRUE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Giacomin
</td>
<td style="text-align:left;">
5
</td>
<td style="text-align:left;">
TRUE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Gilbert
</td>
<td style="text-align:left;">
5
</td>
<td style="text-align:left;">
TRUE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Greiss
</td>
<td style="text-align:left;">
5
</td>
<td style="text-align:left;">
TRUE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Gustavsson
</td>
<td style="text-align:left;">
5
</td>
<td style="text-align:left;">
TRUE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Hall
</td>
<td style="text-align:left;">
5
</td>
<td style="text-align:left;">
TRUE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Hanlon
</td>
<td style="text-align:left;">
5
</td>
<td style="text-align:left;">
TRUE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Hasek
</td>
<td style="text-align:left;">
5
</td>
<td style="text-align:left;">
TRUE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Holmes
</td>
<td style="text-align:left;">
5
</td>
<td style="text-align:left;">
TRUE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Howard
</td>
<td style="text-align:left;">
5
</td>
<td style="text-align:left;">
TRUE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Ing
</td>
<td style="text-align:left;">
5
</td>
<td style="text-align:left;">
TRUE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Joseph
</td>
<td style="text-align:left;">
5
</td>
<td style="text-align:left;">
TRUE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Lamothe
</td>
<td style="text-align:left;">
5
</td>
<td style="text-align:left;">
TRUE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Legace
</td>
<td style="text-align:left;">
5
</td>
<td style="text-align:left;">
TRUE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Low
</td>
<td style="text-align:left;">
5
</td>
<td style="text-align:left;">
TRUE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Lumley
</td>
<td style="text-align:left;">
5
</td>
<td style="text-align:left;">
TRUE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
MacDonald
</td>
<td style="text-align:left;">
5
</td>
<td style="text-align:left;">
TRUE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
McDuffe
</td>
<td style="text-align:left;">
5
</td>
<td style="text-align:left;">
TRUE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Millen
</td>
<td style="text-align:left;">
5
</td>
<td style="text-align:left;">
TRUE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Mio
</td>
<td style="text-align:left;">
5
</td>
<td style="text-align:left;">
TRUE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Moore
</td>
<td style="text-align:left;">
5
</td>
<td style="text-align:left;">
TRUE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Mowers
</td>
<td style="text-align:left;">
5
</td>
<td style="text-align:left;">
TRUE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Osgood
</td>
<td style="text-align:left;">
5
</td>
<td style="text-align:left;">
TRUE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Pickard
</td>
<td style="text-align:left;">
5
</td>
<td style="text-align:left;">
TRUE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Ranford
</td>
<td style="text-align:left;">
5
</td>
<td style="text-align:left;">
TRUE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Riendeau
</td>
<td style="text-align:left;">
5
</td>
<td style="text-align:left;">
TRUE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Roach
</td>
<td style="text-align:left;">
5
</td>
<td style="text-align:left;">
TRUE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Sauve
</td>
<td style="text-align:left;">
5
</td>
<td style="text-align:left;">
TRUE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Sawchuk
</td>
<td style="text-align:left;">
5
</td>
<td style="text-align:left;">
TRUE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Smith
</td>
<td style="text-align:left;">
5
</td>
<td style="text-align:left;">
TRUE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Thompson
</td>
<td style="text-align:left;">
5
</td>
<td style="text-align:left;">
TRUE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Vachon
</td>
<td style="text-align:left;">
5
</td>
<td style="text-align:left;">
TRUE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Vernon
</td>
<td style="text-align:left;">
5
</td>
<td style="text-align:left;">
TRUE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Wregget
</td>
<td style="text-align:left;">
5
</td>
<td style="text-align:left;">
TRUE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Bassen
</td>
<td style="text-align:left;">
6
</td>
<td style="text-align:left;">
TRUE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Bernier
</td>
<td style="text-align:left;">
6
</td>
<td style="text-align:left;">
TRUE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Bester
</td>
<td style="text-align:left;">
6
</td>
<td style="text-align:left;">
TRUE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Bourque
</td>
<td style="text-align:left;">
6
</td>
<td style="text-align:left;">
TRUE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Chevrier
</td>
<td style="text-align:left;">
6
</td>
<td style="text-align:left;">
TRUE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Conklin
</td>
<td style="text-align:left;">
6
</td>
<td style="text-align:left;">
TRUE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Connell
</td>
<td style="text-align:left;">
6
</td>
<td style="text-align:left;">
TRUE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Cox
</td>
<td style="text-align:left;">
6
</td>
<td style="text-align:left;">
TRUE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Crozier
</td>
<td style="text-align:left;">
6
</td>
<td style="text-align:left;">
TRUE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Cude
</td>
<td style="text-align:left;">
6
</td>
<td style="text-align:left;">
TRUE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Daley
</td>
<td style="text-align:left;">
6
</td>
<td style="text-align:left;">
TRUE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
DeJordy
</td>
<td style="text-align:left;">
6
</td>
<td style="text-align:left;">
TRUE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Edwards
</td>
<td style="text-align:left;">
6
</td>
<td style="text-align:left;">
TRUE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Eliot
</td>
<td style="text-align:left;">
6
</td>
<td style="text-align:left;">
TRUE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Essensa
</td>
<td style="text-align:left;">
6
</td>
<td style="text-align:left;">
TRUE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Giacomin
</td>
<td style="text-align:left;">
6
</td>
<td style="text-align:left;">
TRUE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Gilbert
</td>
<td style="text-align:left;">
6
</td>
<td style="text-align:left;">
TRUE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Greiss
</td>
<td style="text-align:left;">
6
</td>
<td style="text-align:left;">
TRUE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Gustavsson
</td>
<td style="text-align:left;">
6
</td>
<td style="text-align:left;">
TRUE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Hall
</td>
<td style="text-align:left;">
6
</td>
<td style="text-align:left;">
TRUE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Hanlon
</td>
<td style="text-align:left;">
6
</td>
<td style="text-align:left;">
TRUE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Hasek
</td>
<td style="text-align:left;">
6
</td>
<td style="text-align:left;">
TRUE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Holmes
</td>
<td style="text-align:left;">
6
</td>
<td style="text-align:left;">
TRUE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Howard
</td>
<td style="text-align:left;">
6
</td>
<td style="text-align:left;">
TRUE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Ing
</td>
<td style="text-align:left;">
6
</td>
<td style="text-align:left;">
TRUE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Joseph
</td>
<td style="text-align:left;">
6
</td>
<td style="text-align:left;">
TRUE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Lamothe
</td>
<td style="text-align:left;">
6
</td>
<td style="text-align:left;">
TRUE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Legace
</td>
<td style="text-align:left;">
6
</td>
<td style="text-align:left;">
TRUE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Low
</td>
<td style="text-align:left;">
6
</td>
<td style="text-align:left;">
TRUE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Lumley
</td>
<td style="text-align:left;">
6
</td>
<td style="text-align:left;">
TRUE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
MacDonald
</td>
<td style="text-align:left;">
6
</td>
<td style="text-align:left;">
TRUE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
McDuffe
</td>
<td style="text-align:left;">
6
</td>
<td style="text-align:left;">
TRUE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Millen
</td>
<td style="text-align:left;">
6
</td>
<td style="text-align:left;">
TRUE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Mio
</td>
<td style="text-align:left;">
6
</td>
<td style="text-align:left;">
TRUE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Moore
</td>
<td style="text-align:left;">
6
</td>
<td style="text-align:left;">
TRUE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Mowers
</td>
<td style="text-align:left;">
6
</td>
<td style="text-align:left;">
TRUE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Osgood
</td>
<td style="text-align:left;">
6
</td>
<td style="text-align:left;">
TRUE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Pickard
</td>
<td style="text-align:left;">
6
</td>
<td style="text-align:left;">
TRUE
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
Ranford
</td>
<td style="text-align:left;">
6
</td>
<td style="text-align:left;">
TRUE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Riendeau
</td>
<td style="text-align:left;">
6
</td>
<td style="text-align:left;">
TRUE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Roach
</td>
<td style="text-align:left;">
6
</td>
<td style="text-align:left;">
TRUE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Sauve
</td>
<td style="text-align:left;">
6
</td>
<td style="text-align:left;">
TRUE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Sawchuk
</td>
<td style="text-align:left;">
6
</td>
<td style="text-align:left;">
TRUE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Smith
</td>
<td style="text-align:left;">
6
</td>
<td style="text-align:left;">
TRUE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Thompson
</td>
<td style="text-align:left;">
6
</td>
<td style="text-align:left;">
TRUE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Vachon
</td>
<td style="text-align:left;">
6
</td>
<td style="text-align:left;">
TRUE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Vernon
</td>
<td style="text-align:left;">
6
</td>
<td style="text-align:left;">
TRUE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Wregget
</td>
<td style="text-align:left;">
6
</td>
<td style="text-align:left;">
TRUE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Bassen
</td>
<td style="text-align:left;">
7
</td>
<td style="text-align:left;">
TRUE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Bernier
</td>
<td style="text-align:left;">
7
</td>
<td style="text-align:left;">
TRUE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Bester
</td>
<td style="text-align:left;">
7
</td>
<td style="text-align:left;">
TRUE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Bourque
</td>
<td style="text-align:left;">
7
</td>
<td style="text-align:left;">
TRUE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Chevrier
</td>
<td style="text-align:left;">
7
</td>
<td style="text-align:left;">
TRUE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Conklin
</td>
<td style="text-align:left;">
7
</td>
<td style="text-align:left;">
TRUE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Connell
</td>
<td style="text-align:left;">
7
</td>
<td style="text-align:left;">
TRUE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Cox
</td>
<td style="text-align:left;">
7
</td>
<td style="text-align:left;">
TRUE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Crozier
</td>
<td style="text-align:left;">
7
</td>
<td style="text-align:left;">
TRUE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Cude
</td>
<td style="text-align:left;">
7
</td>
<td style="text-align:left;">
TRUE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Daley
</td>
<td style="text-align:left;">
7
</td>
<td style="text-align:left;">
TRUE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
DeJordy
</td>
<td style="text-align:left;">
7
</td>
<td style="text-align:left;">
TRUE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Edwards
</td>
<td style="text-align:left;">
7
</td>
<td style="text-align:left;">
TRUE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Eliot
</td>
<td style="text-align:left;">
7
</td>
<td style="text-align:left;">
TRUE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Essensa
</td>
<td style="text-align:left;">
7
</td>
<td style="text-align:left;">
TRUE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Giacomin
</td>
<td style="text-align:left;">
7
</td>
<td style="text-align:left;">
TRUE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Gilbert
</td>
<td style="text-align:left;">
7
</td>
<td style="text-align:left;">
TRUE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Greiss
</td>
<td style="text-align:left;">
7
</td>
<td style="text-align:left;">
TRUE
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
Gustavsson
</td>
<td style="text-align:left;">
7
</td>
<td style="text-align:left;">
TRUE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Hall
</td>
<td style="text-align:left;">
7
</td>
<td style="text-align:left;">
TRUE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Hanlon
</td>
<td style="text-align:left;">
7
</td>
<td style="text-align:left;">
TRUE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Hasek
</td>
<td style="text-align:left;">
7
</td>
<td style="text-align:left;">
TRUE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Holmes
</td>
<td style="text-align:left;">
7
</td>
<td style="text-align:left;">
TRUE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Howard
</td>
<td style="text-align:left;">
7
</td>
<td style="text-align:left;">
TRUE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Ing
</td>
<td style="text-align:left;">
7
</td>
<td style="text-align:left;">
TRUE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Joseph
</td>
<td style="text-align:left;">
7
</td>
<td style="text-align:left;">
TRUE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Lamothe
</td>
<td style="text-align:left;">
7
</td>
<td style="text-align:left;">
TRUE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Legace
</td>
<td style="text-align:left;">
7
</td>
<td style="text-align:left;">
TRUE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Low
</td>
<td style="text-align:left;">
7
</td>
<td style="text-align:left;">
TRUE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Lumley
</td>
<td style="text-align:left;">
7
</td>
<td style="text-align:left;">
TRUE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
MacDonald
</td>
<td style="text-align:left;">
7
</td>
<td style="text-align:left;">
TRUE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
McDuffe
</td>
<td style="text-align:left;">
7
</td>
<td style="text-align:left;">
TRUE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Millen
</td>
<td style="text-align:left;">
7
</td>
<td style="text-align:left;">
TRUE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Mio
</td>
<td style="text-align:left;">
7
</td>
<td style="text-align:left;">
TRUE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Moore
</td>
<td style="text-align:left;">
7
</td>
<td style="text-align:left;">
TRUE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Mowers
</td>
<td style="text-align:left;">
7
</td>
<td style="text-align:left;">
TRUE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Osgood
</td>
<td style="text-align:left;">
7
</td>
<td style="text-align:left;">
TRUE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Pickard
</td>
<td style="text-align:left;">
7
</td>
<td style="text-align:left;">
TRUE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Ranford
</td>
<td style="text-align:left;">
7
</td>
<td style="text-align:left;">
TRUE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Riendeau
</td>
<td style="text-align:left;">
7
</td>
<td style="text-align:left;">
TRUE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Roach
</td>
<td style="text-align:left;">
7
</td>
<td style="text-align:left;">
TRUE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Sauve
</td>
<td style="text-align:left;">
7
</td>
<td style="text-align:left;">
TRUE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Sawchuk
</td>
<td style="text-align:left;">
7
</td>
<td style="text-align:left;">
TRUE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Smith
</td>
<td style="text-align:left;">
7
</td>
<td style="text-align:left;">
TRUE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Thompson
</td>
<td style="text-align:left;">
7
</td>
<td style="text-align:left;">
TRUE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Vachon
</td>
<td style="text-align:left;">
7
</td>
<td style="text-align:left;">
TRUE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Vernon
</td>
<td style="text-align:left;">
7
</td>
<td style="text-align:left;">
TRUE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Wregget
</td>
<td style="text-align:left;">
7
</td>
<td style="text-align:left;">
TRUE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Bassen
</td>
<td style="text-align:left;">
8
</td>
<td style="text-align:left;">
TRUE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Bernier
</td>
<td style="text-align:left;">
8
</td>
<td style="text-align:left;">
TRUE
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
Bester
</td>
<td style="text-align:left;">
8
</td>
<td style="text-align:left;">
TRUE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Bourque
</td>
<td style="text-align:left;">
8
</td>
<td style="text-align:left;">
TRUE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Chevrier
</td>
<td style="text-align:left;">
8
</td>
<td style="text-align:left;">
TRUE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Conklin
</td>
<td style="text-align:left;">
8
</td>
<td style="text-align:left;">
TRUE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Connell
</td>
<td style="text-align:left;">
8
</td>
<td style="text-align:left;">
TRUE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Cox
</td>
<td style="text-align:left;">
8
</td>
<td style="text-align:left;">
TRUE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Crozier
</td>
<td style="text-align:left;">
8
</td>
<td style="text-align:left;">
TRUE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Cude
</td>
<td style="text-align:left;">
8
</td>
<td style="text-align:left;">
TRUE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Daley
</td>
<td style="text-align:left;">
8
</td>
<td style="text-align:left;">
TRUE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
DeJordy
</td>
<td style="text-align:left;">
8
</td>
<td style="text-align:left;">
TRUE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Edwards
</td>
<td style="text-align:left;">
8
</td>
<td style="text-align:left;">
TRUE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Eliot
</td>
<td style="text-align:left;">
8
</td>
<td style="text-align:left;">
TRUE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Essensa
</td>
<td style="text-align:left;">
8
</td>
<td style="text-align:left;">
TRUE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Giacomin
</td>
<td style="text-align:left;">
8
</td>
<td style="text-align:left;">
TRUE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Gilbert
</td>
<td style="text-align:left;">
8
</td>
<td style="text-align:left;">
TRUE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Greiss
</td>
<td style="text-align:left;">
8
</td>
<td style="text-align:left;">
TRUE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Gustavsson
</td>
<td style="text-align:left;">
8
</td>
<td style="text-align:left;">
TRUE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Hall
</td>
<td style="text-align:left;">
8
</td>
<td style="text-align:left;">
TRUE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Hanlon
</td>
<td style="text-align:left;">
8
</td>
<td style="text-align:left;">
TRUE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Hasek
</td>
<td style="text-align:left;">
8
</td>
<td style="text-align:left;">
TRUE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Holmes
</td>
<td style="text-align:left;">
8
</td>
<td style="text-align:left;">
TRUE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Howard
</td>
<td style="text-align:left;">
8
</td>
<td style="text-align:left;">
TRUE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Ing
</td>
<td style="text-align:left;">
8
</td>
<td style="text-align:left;">
TRUE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Joseph
</td>
<td style="text-align:left;">
8
</td>
<td style="text-align:left;">
TRUE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Lamothe
</td>
<td style="text-align:left;">
8
</td>
<td style="text-align:left;">
TRUE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Legace
</td>
<td style="text-align:left;">
8
</td>
<td style="text-align:left;">
TRUE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Low
</td>
<td style="text-align:left;">
8
</td>
<td style="text-align:left;">
TRUE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Lumley
</td>
<td style="text-align:left;">
8
</td>
<td style="text-align:left;">
TRUE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
MacDonald
</td>
<td style="text-align:left;">
8
</td>
<td style="text-align:left;">
TRUE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
McDuffe
</td>
<td style="text-align:left;">
8
</td>
<td style="text-align:left;">
TRUE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Millen
</td>
<td style="text-align:left;">
8
</td>
<td style="text-align:left;">
TRUE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Mio
</td>
<td style="text-align:left;">
8
</td>
<td style="text-align:left;">
TRUE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Moore
</td>
<td style="text-align:left;">
8
</td>
<td style="text-align:left;">
TRUE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Mowers
</td>
<td style="text-align:left;">
8
</td>
<td style="text-align:left;">
TRUE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Osgood
</td>
<td style="text-align:left;">
8
</td>
<td style="text-align:left;">
TRUE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Pickard
</td>
<td style="text-align:left;">
8
</td>
<td style="text-align:left;">
TRUE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Ranford
</td>
<td style="text-align:left;">
8
</td>
<td style="text-align:left;">
TRUE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Riendeau
</td>
<td style="text-align:left;">
8
</td>
<td style="text-align:left;">
TRUE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Roach
</td>
<td style="text-align:left;">
8
</td>
<td style="text-align:left;">
TRUE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Sauve
</td>
<td style="text-align:left;">
8
</td>
<td style="text-align:left;">
TRUE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Sawchuk
</td>
<td style="text-align:left;">
8
</td>
<td style="text-align:left;">
TRUE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Smith
</td>
<td style="text-align:left;">
8
</td>
<td style="text-align:left;">
TRUE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Thompson
</td>
<td style="text-align:left;">
8
</td>
<td style="text-align:left;">
TRUE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Vachon
</td>
<td style="text-align:left;">
8
</td>
<td style="text-align:left;">
TRUE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Vernon
</td>
<td style="text-align:left;">
8
</td>
<td style="text-align:left;">
TRUE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Wregget
</td>
<td style="text-align:left;">
8
</td>
<td style="text-align:left;">
TRUE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Bassen
</td>
<td style="text-align:left;">
9
</td>
<td style="text-align:left;">
TRUE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Bernier
</td>
<td style="text-align:left;">
9
</td>
<td style="text-align:left;">
TRUE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Bester
</td>
<td style="text-align:left;">
9
</td>
<td style="text-align:left;">
TRUE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Bourque
</td>
<td style="text-align:left;">
9
</td>
<td style="text-align:left;">
TRUE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Chevrier
</td>
<td style="text-align:left;">
9
</td>
<td style="text-align:left;">
TRUE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Conklin
</td>
<td style="text-align:left;">
9
</td>
<td style="text-align:left;">
TRUE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Connell
</td>
<td style="text-align:left;">
9
</td>
<td style="text-align:left;">
TRUE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Cox
</td>
<td style="text-align:left;">
9
</td>
<td style="text-align:left;">
TRUE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Crozier
</td>
<td style="text-align:left;">
9
</td>
<td style="text-align:left;">
TRUE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Cude
</td>
<td style="text-align:left;">
9
</td>
<td style="text-align:left;">
TRUE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Daley
</td>
<td style="text-align:left;">
9
</td>
<td style="text-align:left;">
TRUE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
DeJordy
</td>
<td style="text-align:left;">
9
</td>
<td style="text-align:left;">
TRUE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Edwards
</td>
<td style="text-align:left;">
9
</td>
<td style="text-align:left;">
TRUE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Eliot
</td>
<td style="text-align:left;">
9
</td>
<td style="text-align:left;">
TRUE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Essensa
</td>
<td style="text-align:left;">
9
</td>
<td style="text-align:left;">
TRUE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Giacomin
</td>
<td style="text-align:left;">
9
</td>
<td style="text-align:left;">
TRUE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Gilbert
</td>
<td style="text-align:left;">
9
</td>
<td style="text-align:left;">
TRUE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Greiss
</td>
<td style="text-align:left;">
9
</td>
<td style="text-align:left;">
TRUE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Gustavsson
</td>
<td style="text-align:left;">
9
</td>
<td style="text-align:left;">
TRUE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Hall
</td>
<td style="text-align:left;">
9
</td>
<td style="text-align:left;">
TRUE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Hanlon
</td>
<td style="text-align:left;">
9
</td>
<td style="text-align:left;">
TRUE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Hasek
</td>
<td style="text-align:left;">
9
</td>
<td style="text-align:left;">
TRUE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Holmes
</td>
<td style="text-align:left;">
9
</td>
<td style="text-align:left;">
TRUE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Howard
</td>
<td style="text-align:left;">
9
</td>
<td style="text-align:left;">
TRUE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Ing
</td>
<td style="text-align:left;">
9
</td>
<td style="text-align:left;">
TRUE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Joseph
</td>
<td style="text-align:left;">
9
</td>
<td style="text-align:left;">
TRUE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Lamothe
</td>
<td style="text-align:left;">
9
</td>
<td style="text-align:left;">
TRUE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Legace
</td>
<td style="text-align:left;">
9
</td>
<td style="text-align:left;">
TRUE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Low
</td>
<td style="text-align:left;">
9
</td>
<td style="text-align:left;">
TRUE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Lumley
</td>
<td style="text-align:left;">
9
</td>
<td style="text-align:left;">
TRUE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
MacDonald
</td>
<td style="text-align:left;">
9
</td>
<td style="text-align:left;">
TRUE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
McDuffe
</td>
<td style="text-align:left;">
9
</td>
<td style="text-align:left;">
TRUE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Millen
</td>
<td style="text-align:left;">
9
</td>
<td style="text-align:left;">
TRUE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Mio
</td>
<td style="text-align:left;">
9
</td>
<td style="text-align:left;">
TRUE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Moore
</td>
<td style="text-align:left;">
9
</td>
<td style="text-align:left;">
TRUE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Mowers
</td>
<td style="text-align:left;">
9
</td>
<td style="text-align:left;">
TRUE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Osgood
</td>
<td style="text-align:left;">
9
</td>
<td style="text-align:left;">
TRUE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Pickard
</td>
<td style="text-align:left;">
9
</td>
<td style="text-align:left;">
TRUE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Ranford
</td>
<td style="text-align:left;">
9
</td>
<td style="text-align:left;">
TRUE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Riendeau
</td>
<td style="text-align:left;">
9
</td>
<td style="text-align:left;">
TRUE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Roach
</td>
<td style="text-align:left;">
9
</td>
<td style="text-align:left;">
TRUE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Sauve
</td>
<td style="text-align:left;">
9
</td>
<td style="text-align:left;">
TRUE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Sawchuk
</td>
<td style="text-align:left;">
9
</td>
<td style="text-align:left;">
TRUE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Smith
</td>
<td style="text-align:left;">
9
</td>
<td style="text-align:left;">
TRUE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Thompson
</td>
<td style="text-align:left;">
9
</td>
<td style="text-align:left;">
TRUE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Vachon
</td>
<td style="text-align:left;">
9
</td>
<td style="text-align:left;">
TRUE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Vernon
</td>
<td style="text-align:left;">
9
</td>
<td style="text-align:left;">
TRUE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Wregget
</td>
<td style="text-align:left;">
9
</td>
<td style="text-align:left;">
TRUE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Bassen
</td>
<td style="text-align:left;">
10
</td>
<td style="text-align:left;">
TRUE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Bernier
</td>
<td style="text-align:left;">
10
</td>
<td style="text-align:left;">
TRUE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Bester
</td>
<td style="text-align:left;">
10
</td>
<td style="text-align:left;">
TRUE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Bourque
</td>
<td style="text-align:left;">
10
</td>
<td style="text-align:left;">
TRUE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Chevrier
</td>
<td style="text-align:left;">
10
</td>
<td style="text-align:left;">
TRUE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Conklin
</td>
<td style="text-align:left;">
10
</td>
<td style="text-align:left;">
TRUE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Connell
</td>
<td style="text-align:left;">
10
</td>
<td style="text-align:left;">
TRUE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Cox
</td>
<td style="text-align:left;">
10
</td>
<td style="text-align:left;">
TRUE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Crozier
</td>
<td style="text-align:left;">
10
</td>
<td style="text-align:left;">
TRUE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Cude
</td>
<td style="text-align:left;">
10
</td>
<td style="text-align:left;">
TRUE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Daley
</td>
<td style="text-align:left;">
10
</td>
<td style="text-align:left;">
TRUE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
DeJordy
</td>
<td style="text-align:left;">
10
</td>
<td style="text-align:left;">
TRUE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Edwards
</td>
<td style="text-align:left;">
10
</td>
<td style="text-align:left;">
TRUE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Eliot
</td>
<td style="text-align:left;">
10
</td>
<td style="text-align:left;">
TRUE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Essensa
</td>
<td style="text-align:left;">
10
</td>
<td style="text-align:left;">
TRUE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Giacomin
</td>
<td style="text-align:left;">
10
</td>
<td style="text-align:left;">
TRUE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Gilbert
</td>
<td style="text-align:left;">
10
</td>
<td style="text-align:left;">
TRUE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Greiss
</td>
<td style="text-align:left;">
10
</td>
<td style="text-align:left;">
TRUE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Gustavsson
</td>
<td style="text-align:left;">
10
</td>
<td style="text-align:left;">
TRUE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Hall
</td>
<td style="text-align:left;">
10
</td>
<td style="text-align:left;">
TRUE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Hanlon
</td>
<td style="text-align:left;">
10
</td>
<td style="text-align:left;">
TRUE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Hasek
</td>
<td style="text-align:left;">
10
</td>
<td style="text-align:left;">
TRUE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Holmes
</td>
<td style="text-align:left;">
10
</td>
<td style="text-align:left;">
TRUE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Howard
</td>
<td style="text-align:left;">
10
</td>
<td style="text-align:left;">
TRUE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Ing
</td>
<td style="text-align:left;">
10
</td>
<td style="text-align:left;">
TRUE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Joseph
</td>
<td style="text-align:left;">
10
</td>
<td style="text-align:left;">
TRUE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Lamothe
</td>
<td style="text-align:left;">
10
</td>
<td style="text-align:left;">
TRUE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Legace
</td>
<td style="text-align:left;">
10
</td>
<td style="text-align:left;">
TRUE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Low
</td>
<td style="text-align:left;">
10
</td>
<td style="text-align:left;">
TRUE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Lumley
</td>
<td style="text-align:left;">
10
</td>
<td style="text-align:left;">
TRUE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
MacDonald
</td>
<td style="text-align:left;">
10
</td>
<td style="text-align:left;">
TRUE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
McDuffe
</td>
<td style="text-align:left;">
10
</td>
<td style="text-align:left;">
TRUE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Millen
</td>
<td style="text-align:left;">
10
</td>
<td style="text-align:left;">
TRUE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Mio
</td>
<td style="text-align:left;">
10
</td>
<td style="text-align:left;">
TRUE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Moore
</td>
<td style="text-align:left;">
10
</td>
<td style="text-align:left;">
TRUE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Mowers
</td>
<td style="text-align:left;">
10
</td>
<td style="text-align:left;">
TRUE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Osgood
</td>
<td style="text-align:left;">
10
</td>
<td style="text-align:left;">
TRUE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Pickard
</td>
<td style="text-align:left;">
10
</td>
<td style="text-align:left;">
TRUE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Ranford
</td>
<td style="text-align:left;">
10
</td>
<td style="text-align:left;">
TRUE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Riendeau
</td>
<td style="text-align:left;">
10
</td>
<td style="text-align:left;">
TRUE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Roach
</td>
<td style="text-align:left;">
10
</td>
<td style="text-align:left;">
TRUE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Sauve
</td>
<td style="text-align:left;">
10
</td>
<td style="text-align:left;">
TRUE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Sawchuk
</td>
<td style="text-align:left;">
10
</td>
<td style="text-align:left;">
TRUE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Smith
</td>
<td style="text-align:left;">
10
</td>
<td style="text-align:left;">
TRUE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Thompson
</td>
<td style="text-align:left;">
10
</td>
<td style="text-align:left;">
TRUE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Vachon
</td>
<td style="text-align:left;">
10
</td>
<td style="text-align:left;">
TRUE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Vernon
</td>
<td style="text-align:left;">
10
</td>
<td style="text-align:left;">
TRUE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Wregget
</td>
<td style="text-align:left;">
10
</td>
<td style="text-align:left;">
TRUE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Bassen
</td>
<td style="text-align:left;">
11
</td>
<td style="text-align:left;">
TRUE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Bernier
</td>
<td style="text-align:left;">
11
</td>
<td style="text-align:left;">
TRUE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Bester
</td>
<td style="text-align:left;">
11
</td>
<td style="text-align:left;">
TRUE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Bourque
</td>
<td style="text-align:left;">
11
</td>
<td style="text-align:left;">
TRUE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Chevrier
</td>
<td style="text-align:left;">
11
</td>
<td style="text-align:left;">
TRUE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Conklin
</td>
<td style="text-align:left;">
11
</td>
<td style="text-align:left;">
TRUE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Connell
</td>
<td style="text-align:left;">
11
</td>
<td style="text-align:left;">
TRUE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Cox
</td>
<td style="text-align:left;">
11
</td>
<td style="text-align:left;">
TRUE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Crozier
</td>
<td style="text-align:left;">
11
</td>
<td style="text-align:left;">
TRUE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Cude
</td>
<td style="text-align:left;">
11
</td>
<td style="text-align:left;">
TRUE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Daley
</td>
<td style="text-align:left;">
11
</td>
<td style="text-align:left;">
TRUE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
DeJordy
</td>
<td style="text-align:left;">
11
</td>
<td style="text-align:left;">
TRUE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Edwards
</td>
<td style="text-align:left;">
11
</td>
<td style="text-align:left;">
TRUE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Eliot
</td>
<td style="text-align:left;">
11
</td>
<td style="text-align:left;">
TRUE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Essensa
</td>
<td style="text-align:left;">
11
</td>
<td style="text-align:left;">
TRUE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Giacomin
</td>
<td style="text-align:left;">
11
</td>
<td style="text-align:left;">
TRUE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Gilbert
</td>
<td style="text-align:left;">
11
</td>
<td style="text-align:left;">
TRUE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Greiss
</td>
<td style="text-align:left;">
11
</td>
<td style="text-align:left;">
TRUE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Gustavsson
</td>
<td style="text-align:left;">
11
</td>
<td style="text-align:left;">
TRUE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Hall
</td>
<td style="text-align:left;">
11
</td>
<td style="text-align:left;">
TRUE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Hanlon
</td>
<td style="text-align:left;">
11
</td>
<td style="text-align:left;">
TRUE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Hasek
</td>
<td style="text-align:left;">
11
</td>
<td style="text-align:left;">
TRUE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Holmes
</td>
<td style="text-align:left;">
11
</td>
<td style="text-align:left;">
TRUE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Howard
</td>
<td style="text-align:left;">
11
</td>
<td style="text-align:left;">
TRUE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Ing
</td>
<td style="text-align:left;">
11
</td>
<td style="text-align:left;">
TRUE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Joseph
</td>
<td style="text-align:left;">
11
</td>
<td style="text-align:left;">
TRUE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Lamothe
</td>
<td style="text-align:left;">
11
</td>
<td style="text-align:left;">
TRUE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Legace
</td>
<td style="text-align:left;">
11
</td>
<td style="text-align:left;">
TRUE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Low
</td>
<td style="text-align:left;">
11
</td>
<td style="text-align:left;">
TRUE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Lumley
</td>
<td style="text-align:left;">
11
</td>
<td style="text-align:left;">
TRUE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
MacDonald
</td>
<td style="text-align:left;">
11
</td>
<td style="text-align:left;">
TRUE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
McDuffe
</td>
<td style="text-align:left;">
11
</td>
<td style="text-align:left;">
TRUE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Millen
</td>
<td style="text-align:left;">
11
</td>
<td style="text-align:left;">
TRUE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Mio
</td>
<td style="text-align:left;">
11
</td>
<td style="text-align:left;">
TRUE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Moore
</td>
<td style="text-align:left;">
11
</td>
<td style="text-align:left;">
TRUE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Mowers
</td>
<td style="text-align:left;">
11
</td>
<td style="text-align:left;">
TRUE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Osgood
</td>
<td style="text-align:left;">
11
</td>
<td style="text-align:left;">
TRUE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Pickard
</td>
<td style="text-align:left;">
11
</td>
<td style="text-align:left;">
TRUE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Ranford
</td>
<td style="text-align:left;">
11
</td>
<td style="text-align:left;">
TRUE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Riendeau
</td>
<td style="text-align:left;">
11
</td>
<td style="text-align:left;">
TRUE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Roach
</td>
<td style="text-align:left;">
11
</td>
<td style="text-align:left;">
TRUE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Sauve
</td>
<td style="text-align:left;">
11
</td>
<td style="text-align:left;">
TRUE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Sawchuk
</td>
<td style="text-align:left;">
11
</td>
<td style="text-align:left;">
TRUE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Smith
</td>
<td style="text-align:left;">
11
</td>
<td style="text-align:left;">
TRUE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Thompson
</td>
<td style="text-align:left;">
11
</td>
<td style="text-align:left;">
TRUE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Vachon
</td>
<td style="text-align:left;">
11
</td>
<td style="text-align:left;">
TRUE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Vernon
</td>
<td style="text-align:left;">
11
</td>
<td style="text-align:left;">
TRUE
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Wregget
</td>
<td style="text-align:left;">
11
</td>
<td style="text-align:left;">
TRUE
</td>
<td style="text-align:right;">
0
</td>
</tr>
</tbody>
</table>

### Numerical summaries for some quantitative variables

Created Function to get data filtered having more than 100 games. User
can mention this input value which he wants to generate the game
summary. Subset goli 12 data for games played by goli 12, winRate and
lossRate which are newly created 2 columns.

``` r
gameSummary <- function(moreThanGames) {
 goli_12_game_smry <- goli_12_data %>% filter(data.gamesPlayed > moreThanGames) %>% select(data.firstName,data.gamesPlayed, winRate, lossRate)
}
goli_12_game_smry <- gameSummary(100)
goli_12_game_smry
```

    ##    data.firstName data.gamesPlayed winRate lossRate
    ## 1            Glen              186    0.35     0.38
    ## 2         Dominik              176    0.65     0.22
    ## 3           Roger              310    0.42     0.38
    ## 4             Roy              221    0.43     0.37
    ## 5           Glenn              148    0.50     0.30
    ## 6           Harry              324    0.50     0.32
    ## 7          Johnny              152    0.43     0.40
    ## 8           Terry              734    0.48     0.33
    ## 9          Normie              178    0.43     0.40
    ## 10          Rogie              109    0.28     0.52
    ## 11          Chris              565    0.56     0.26
    ## 12          Manny              180    0.62     0.19
    ## 13          Jimmy              543    0.45     0.36
    ## 14       Jonathan              105    0.31     0.49

Summary, min/max, median, mean, Q1/Q3 is generated based on above
filtered data. On an average 280 games played with max win rate.

``` r
getSummary <- function() {
  goli_12_game_smry<- descr(goli_12_game_smry,stats = c("min","Q1","med", "mean", "Q3", "max"))
  goli_12_game_smry <- data.frame(goli_12_game_smry)
  goli_12_game_smry <- round(goli_12_game_smry, digits = 1)
  return(goli_12_game_smry)
}
  getSummary()
```

    ##        data.gamesPlayed lossRate winRate
    ## Min               105.0      0.2     0.3
    ## Q1                152.0      0.3     0.4
    ## Median            183.0      0.4     0.4
    ## Mean              280.8      0.4     0.5
    ## Q3                324.0      0.4     0.5
    ## Max               734.0      0.5     0.7

## PLOTS :

### 1) Bar plot : Active/inactive players Bar plot

This is Bar plot created for active player using geom\_bar function.
Counts for the inactive player is way more than active player.

``` r
goli_12_box <- ggplot(data=goli_12_data, aes(x=data.activePlayer))
goli_12_box + geom_bar(aes(fill= data.activePlayer), position = "dodge") + labs(title = "Active/inactive players Bar plot")
```

![](git_doc_output_files/figure-gfm/unnamed-chunk-10-1.png)<!-- -->

### 2) Histogram Plot : Histogram Plot for goli 12 games played and win rate.

The observation is we see win rate is more in the beginning. As the
games are increasing, winning rate is decreasing.

``` r
ggplot(goli_12_data, aes(x=data.gamesPlayed)) + geom_histogram(aes(y = ..density.., fill = winRate), bins = 26) + geom_density(adjust=0.4, alpha=0.4, color = "red", size = 2,outline.type="full", position = "stack") + labs(title="Histogram for Goli 12 win rate")
```

![](git_doc_output_files/figure-gfm/unnamed-chunk-11-1.png)<!-- -->

### 3) Scatterplot : Games Played vs Win Rate

Scatterplot is a good visualization tool. It is comparing games played
vs win rate for goli 12.Win rate is seems to be decreased for Goli 12 as
the number of games increased.

![](git_doc_output_files/figure-gfm/unnamed-chunk-12-1.png)<!-- -->

### 4) ScatterP Plot : Games played vs Wins

The active players has larger width, which can be the effect of very
lower number of players .I used geom\_point here. The point geom is used
to create scatterplots.

``` r
ggplot (goli_12_data, aes(x=data.gamesPlayed, y=data.wins, group=data.activePlayer)) + geom_point(aes(color= data.activePlayer)) + geom_smooth(method='lm', color='light blue') + ggtitle("Games played vs Wins")
```

![](git_doc_output_files/figure-gfm/unnamed-chunk-13-1.png)<!-- -->

### 5) Box Plot: Boxplot of games played and losses by individual players

More games are loose by inactive players where as oberving at median,
high number of games are lost by active players

``` r
goli_12_box2<- ggplot(data= goli_12_data, aes(x=data.gamesPlayed, y= data.losses, group=data.activePlayer, color=data.activePlayer))
goli_12_box2 + geom_boxplot() + labs(title="Boxplot of games played and losses by individual players") + geom_jitter(aes(color=data.activePlayer))
```

![](git_doc_output_files/figure-gfm/unnamed-chunk-14-1.png)<!-- -->

### 6) Histogram Plot : Histogram for games Played

Two colors shows different categores - active and inactive players.
Inactive players have played large number of games. This graph is right
skewed.Large number of players played less than 5 games.

``` r
goli_12_box_data <- goli_12_data %>% select(data.activePlayer, data.gamesPlayed) 
histogram1<- ggplot(data=goli_12_box_data, aes(x=data.gamesPlayed))
histogram1 + geom_histogram(binwidth = 10, aes(fill= data.activePlayer)) + labs(title="Histogram for games Played") +
  geom_density(adjust= 0.25, alpha=0.05)
```

![](git_doc_output_files/figure-gfm/unnamed-chunk-15-1.png)<!-- -->
