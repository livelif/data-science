# Introduction
Today I will get some insights using the Cartola dataset (https://www.kaggle.com/lgmoneda/cartola-fc-brasil-scouts?select=jogadores.csv). 
Cartola is a game that gets data of Brasileirao (Brazilian Soccer Championship) and the user can buy a player and score with them
When I saw my friends playing Cartola game and making decisions based on insights without statistics or data. So, I thought: “Can I make a decision in Cartola based on data?”
Another important thing is the movie Moneyball, where the manager buys players based on statistics reaching the championship final. Can we do something similar? Look, I have price, profit, and score… I will try it.
Before I start I will check some details, when you see ZAG in a graph it means defender, check the examples:
* ATA = Attackers
* ZAG = Defenders
* TEC = Manager
* LAT = Lateral
* MEI = Mid
* GOL = Goalkeeper

# Libraries
For this project I use matplotlib, seaborn, pandas and numpy

# Motivation
The main motivation is learn data science and the libraries. Secondly, I wanted to extract some statistics and data to try to build a competitive team at Cartola using data

# Results
The EV team for this case, although more profitable in the long run, does not pay, because we want to score more even by risking more and losing cartouches. We also saw that putting together a team with a very high average is out of scope because it requires great value for cartoletas. We also found that scaling lateral, defenders and mids is better than investing in expensive attackers.
A very important note is that these algorithms do not have a team captain whose score is always 2x.

# Important
For more details check my blog post https://paulo-bernardo.medium.com/lets-analyze-the-data-of-brazilian-soccer-f5030c251010 