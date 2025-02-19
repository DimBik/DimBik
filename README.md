# Data Scientist

#### Technical Skills: Python, SQL, R, JavaScript, HTML, CSS, MATLAB, C#, MongoDB, Hadoop, Neo4j, Spark

## Education

MSc. Data Science & Analytics | Munster Technological University (November 2022)

BSc. Mathematics | The National & Kapodistrian University of Athens (July 2020)

## Projects
### <ins>Enhancing Real-Time GI Tract Pathology Detection</ins>
[Code](https://github.com/DimBik/DimBik/blob/main/Projects/Enhancing%20Real-Time%20GI%20Tract%20Pathology%20Detection.ipynb)

Colorectal, esophageal and stomach cancers cause 1.8 million deaths each year, making endoscopic examination crucial for examining the GI tract. Gastroscopy and Colonoscopy are real-time video examinations using high-definition digital endoscopes to inspect the GI tract. Accurate disease assessment is important as it affects treatment and follow-up decisions. For example, the level of inflammation determines the therapy for inflammatory bowel diseases (IBD). This project aims to create a reliable model which recognises 3 major pathological findings in real-time video. The dataset consists of the 8 following classes:

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

![Confusion Matrix](/Photos/conf_mat.png)

**_Best accuracy: 81.5%_**


### <ins>Visualising Tweets</ins>
[Code](https://github.com/DimBik/DimBik/blob/main/Projects/Visualising%20Tweets.ipynb)

In this project, I developed a Shiny Application in R designed to perform data analysis on tweets. The dataset includes multiple attributes such as:
- Time -> Showing the datetime that the tweet was published.
- Reach -> Measuring the visibility of each tweet.
- Number of Retweets -> Showing the number of retweets.
- Likes -> Describing the number of likes of each tweet.
- Klout -> The Klout Score which describes the influence of each tweet.
- Location -> The location of the tweet: City, Country.
- Gender -> Showing the gender of the user.

The app is structured into four interactive tabs, each of them, serving a different purpose in exploring and analysing the dataset.

**_Tab 1: Tweet Location Map_**

For the first tab, the longitude and latitude were retreived from a local dataset of R and merged with the tweets dataset. The coordinates of some cities were manually initialised from Google Maps. All the tweets coming from a city X have the same coordinates, and therefore a random number was added to the longitude and latitude to avoid duplicated points on the map and distribute them around a city X. Below, the first tab is shown.

![tab1](/Photos/tab1.png)

By clicking on a marker, users can access detailed information about a tweet and a user, including gender, Klout score, sentiment score, and other relevant metrics. Additionally, this map is highly interactive, and users can filter the data based on the gender.

**_Tab 2: Country Statistics Map_**

The second tab presents a high-level overview with a polygon map displaying aggregated statistics for each country. Users can click on a specific country to see average sentiment score, average reach score, and other relevant statistics such as minimum, maximum and median. This feature helps indentify local trends and patterns in tweets. The second tab is shown below.

![tab2](/Photos/tab2.png)

**_Tab 3: Multivariate Analysis_**

The third tab is dedicated to multivariate analysis with scatter plots. The user can select different combination of variables and explore relationships between them. The radius of each point on the scatter plot varies according to the value of the selected variable (e.g. Likes) and the colour of each point shows the sentiment score. The user can also apply different transformations on the data. This tab provides a useful tool for in-depth data analysis and the tab is shown below.

![tab3](/Photos/tab3.png)

**_Tab 4: Time Series_**

The fourth tab focuses more on time series analysis displaying different time series plots for each variable. Users can analyse how the number of tweets changes with the day (More tweets on the weekends). They can also analyse the trend, the seasonality and more. This tab provides a flexible tool for analysing how the users' engangement fluctuates over different periods. The last tab is shown below. 

![tab4](/Photos/tab4.png)

## Talks & Lectures
- AI Risks & Risk Management - Multiplier event for the REGULAITE project, Arcos de Valdevez, Portugal, February 2024
- Analysis of feedback from pilot test's questionnaires - Multiplier event for the REGULAITE project, Arcos de Valdevez, Portugal, February 2024

## Certification
AI-Powered Production Scheduling with Constraint Programming - University College Cork (UCC)
