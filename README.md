# Data Scientist

#### Technical Skills: Python, SQL, R, MATLAB, C#, MongoDB, Hadoop, Neo4j, Spark

## Education

MSc. Data Science & Analytics | Munster Technological University (November 2022)

BSc. Mathematics | The National & Kapodistrian University of Athens (July 2020)


## Work Experience
**Research Assistant @ University College Cork (_June 2023 - Present_)**
- Uncovered and corrected missing step in production data pipeline which impacted over 70% of active accounts
- Redeveloped loan originations model which resulted in 50% improvement in model performance and saving 1 million dollars in potential losses

**Annotation Analyst @ Apple (_January 2023 - June 2023_)**
- Conducted data collection, processing, and analysis for novel study evaluating the impact of over 300 biometrics variables on human performance in hyper-realistic, live-fire training scenarios
- Applied unsupervised deep learning approaches to longitudinal ICU data to discover novel sepsis sub-phenotypes

**Mathematics Teacher @ Eumathia Private Tutoring School (_September 2014 - June 2021_)**
- Conducted data collection, processing, and analysis for novel study evaluating the impact of over 300 biometrics variables on human performance in hyper-realistic, live-fire training scenarios
- Applied unsupervised deep learning approaches to longitudinal ICU data to discover novel sepsis sub-phenotypes

## Projects
### Enhancing Real-Time GI Tract Pathology Detection
[Code](https://github.com/DimBik/DimBik/blob/main/Projects/Enhancing%20Real-Time%20GI%20Tract%20Pathology%20Detection.ipynb)

Colorectal, esophageal and stomach cancers cause 1.8 million deaths each year, making endoscopic examination crucial for examining the GI tract. Gastroscopy and Colonoscopy are real-time video examinations using high-definition degital endoscopes to inspect the GI tract. Accurate disease assessment is important as it affects treatment and follow-up decisions. For example, the level of inflammation determines the therapy for inflammatory bowel diseases (IBD). This project aims to create a reliable model which recognises 3 major pathological findings in real-time video. The dataset consists of the 8 following classes:

- Z-line -> Marks the transition between the esophagus and the stomach.
- Esophagitis -> Inflammation of the esophagus visible as a break in the esophageal mucosa near the Z-line.
- Pylorus -> The area around the opening from the stomach into the first part of the small bowel (duodenum).
- Polyps -> Lesions within the bowel detectable as mucosal outgrows.
- Cecum -> The most proximal part of the large bowel.
- Ulcerative Colitis -> A chronic inflammatory disease affecting the large bowel.
- Dyed and Lifted Polyps -> Light blue polyp margins are clearly visible against the darker normal mucosa.
- Dyed resection Margins -> The resection margins are important in order to evaluate whether the polyp is completely removed.

![8 Classes](/Photos/output.png)

The models were evaluated with 5-fold CV. An initial baseline model with an accuracy of 72.5% was built and gradually improved to reach 81.5%. In this project, issues such as bias from the green squares and limited data were addressed.

**_Best accuracy: 81.5%_**


### Data Visualisation on Tweets
[Code](https://github.com/DimBik/DimBik/blob/main/Projects/Visualising%20Tweets.ipynb)

In this project, I developed a Shiny Application with R to perform data analysis on tweets. The dataset includes multiple attributes sich as:
- Time -> Showing the datetime that the tweet was published.
- Reach -> Measuring the visibility of each tweet.
- Number of Retweets -> Showing the number of retweets.
- Likes -> Describing the number of likes of each tweet.
- Klout -> The Klout Score which describes the influence of each tweet.
- Location -> The location of the tweet: City, Country.
- Gender -> Showing the gender of the user.

The app is structured into four interactive tabs, each of them, serving a different purpose in exploring and analysing the dataset.

**_Tab 1: Tweet Location Map_**

For the first tab, the longitude and latitude has been retreived from a local dataset of R and merged with our dataset. The coordinates of some cities have been initialised manually from Google Maps. All the tweets from a city X have the same coordinates and therefore a random number has been added to the longitude and latitude to avoid duplicated points on the map and distribute them around the city X. Below we see the first tab.

![tab1](/Photos/tab1.png)

By clicking on a marker, users can access detailed information about the tweet and the user, including gender, Klout score, sentiment score, and other relevant metrics. Additionally, this map is highly interactive, and users can filter the data based on the gender.

**_Tab 2: Country Statistics Map_**

The second tab presents a high-level overview with a polygon map displaying aggregated statistics for each country. Users can click on a specific country to see average sentiment score, average reach score, and other relevant statistics such as minimum, maximum and median. This feature helps indentify local trends and patterns in tweets. The second tab is shown below.

![tab2](/Photos/tab2.png)

**_Tab 3: Multivariate Analysis_**

The third tab is dedicated to multivariate analysis with scatter plots. The user can select different combination of variables and explore relationships between them. The radius of each point on the scatter plot varies according to the value of the selected variable (ie. Likes) and the colour of each point shows the sentiment score. The user can also apply different transformations on the data. This tab provides a useful tool for in-depth data analysis and the tab is shown below.

![tab3](/Photos/tab3.png)

**_Tab 4: Time Series_**

The fourth tab focuses more on time series analysis displaying different time series plots for each variable. Users can analyse how the number of tweets changes with the time and the day (More tweets on the weekends). They can also analyse the trend, the seasonality and more in the other variables as well. This tab provides a flexible tool for analysing how the users' engangement fluctuates over different periods. The last tab is shown below. 

![tab4](/Photos/tab4.png)


### Classification of real and fake facial images
[Code](https://www.mdpi.com/1424-8220/22/11/4240)

Used **Matlab** to train over 100 machine learning models which estimated particulate matter concentrations based on a suite of over 300 biometric variables. We found biometric variables can be used to accurately estimate particulate matter concentrations at ultra-fine spatial scales with high fidelity (r2 = 0.91) and that smaller particles are better estimated than larger ones. Inferring environmental conditions solely from biometric measurements allows us to disentangle key interactions between the environment and the body.

### Identify LLM-generated text
[Code](https://www.mdpi.com/1424-8220/22/11/4240)

Used **Matlab** to train over 100 machine learning models which estimated particulate matter concentrations based on a suite of over 300 biometric variables. We found biometric variables can be used to accurately estimate particulate matter concentrations at ultra-fine spatial scales with high fidelity (r2 = 0.91) and that smaller particles are better estimated than larger ones. Inferring environmental conditions solely from biometric measurements allows us to disentangle key interactions between the environment and the body.

### Quality analysis of a drug and failure detection
[Code](https://www.mdpi.com/1424-8220/22/11/4240)

Used **Matlab** to train over 100 machine learning models which estimated particulate matter concentrations based on a suite of over 300 biometric variables. We found biometric variables can be used to accurately estimate particulate matter concentrations at ultra-fine spatial scales with high fidelity (r2 = 0.91) and that smaller particles are better estimated than larger ones. Inferring environmental conditions solely from biometric measurements allows us to disentangle key interactions between the environment and the body.

![Bike Study](/assets/img/bike_study.jpeg)

## Talks & Lectures
- AI Risks & Risk Management - GSP Seminar, Fall 2021

## Publications
1. Castane, G.G., Martinez, A., Radaman, Q., Gkika, Z., Panagiotis, M., Vyhmeister, E. Market Analysis of a Data Platform in the European Data Ecosystem (2024). DOI: 10.1007/978-3-031-63227-3__7
