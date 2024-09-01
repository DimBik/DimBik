# Data Scientist

#### Technical Skills: Python, SQL, R, MATLAB, C#, MongoDB, Hadoop, Neo4j, Spark

## Education

MSc. Data Science & Analytics | Munster Technological University (November 2022)

BSc. Mathematics | The National & Kapodistrian University of Athens (July 2020)


## Work Experience
**Research Assistant @ University College Cork (_June 2023 - Present_)**
- Developed a hiring classifier with Python, reducing demographic parity to 0.01, mitigating bias and enhancing fairness in recruitment decisions.
- Employed a data cleaning pipeline with Python to handle inconsistencies in a large dataset, improving accuracy for student performance predictions by 7\%.
- Collaborated with cross-functional teams to design questionnaires for a European project course and perform data analysis with Python, leading to a 1-point improvement in the course's effectiveness rating after implementing identified changes.
- Conducted data analysis with Excel on European data portals to identify the best fit for a European project's platform. Developed the back-end of a Python Flask API to translate and publish metadata to the selected portal.

**Annotation Analyst @ Apple (_January 2023 - June 2023_)**
- Complete more than 1000 daily tasks, improving the overall performance of the team to become one of the top 5 most productive teams within Apple.
- Collaborate within a 6-member team to address ambiguities in the data.

**Mathematics Teacher @ Eumathia Private Tutoring School (_September 2014 - June 2021_)**
- Deliver more than 7 effective presentations per day to convey complex mathematical concepts and bridge knowledge gaps, enhancing my ability to clearly communicate complex concepts to a non-technical audience.

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


### <ins>Classification of real and fake facial images</ins>
[Thesis/Code](https://github.com/DimBik/DimBik/blob/main/Projects/Classification%20of%20real%20and%20fake%20facial%20images.pdf)

Banks are being targeted by fraudsters, leading to a remarkably high annual financial loss. Detecting these fraudsters is the most important goal for banks, as it ensures reliability and security. Over the last 10 years, machine learning and deep learning have performed immensely well in various fields. Big data is now easily handled by stronger hardware systems compared to the past, making more information available for tackling problems that humans cannot. The influence of technology has created a world that allows several processes to be performed efficiently remotely.

![data](/Photos/facial.png)

When a new account is created, banks face the risk of fraud unless regulations are satisfied. Some of the required documents for opening a new account include a photograph of the customer's ID card and an image displaying their face. The documents submitted by the prospective customer are checked by the bank and either accepted or rejected. Fraudsters often submit fake ID cards or photoshopped images to conceal their identity. In this research, the study of photoshopped images to identify potential frauds will be proposed.

![imbalance](/Photos/imb_mean.png)

ConvNeXts have demonstrated excellent performance in image classification tasks, and for this research, they will be included in a voting classifier and trained on an imbalanced dataset of facial images. An imbalanced dataset is used to reflect real-world conditions. The voting classifier, composed of four ConvNeXt models and an ensemble model, successfully classified **_91.67%_** of the fake images.

### <ins>Identify LLM-generated text</ins>
[Code](https://github.com/DimBik/DimBik/blob/main/Projects/Identify%20LLM%20generated%20text.ipynb)

In the era of chatbots, AI-generated text is becoming increasingly sophisticated, distinguishing between human and LLM-generated content has become challenging. In educational perspective, academic integrity ensures that students work on their own. Therefore, in this project, I developed a machine learning model capable of identifying whether an essay is written by a human or LLM.

The initial dataset was limited to LLM-generated text, therefore AI-generated essays were added and data augmentation was performed. Data cleaning pipelines were implemented to enhance data quality, including stemming and deletion of special characters, numbers, punctuations and stop words. TF-IDF vectoriser was used, and preliminary external analysis showed that LLM uses shorter text than humans. Therefore an attribute which keeps tracking the number of words was included on the dataset. Lastly, unique words analysis for each class was conducted which shows the difference between human creativity and machine generation.

The model should confidently indentify whether the essay is written by a human or not, and therefore the KPI to assess the model's performance is **_AUC_**.

**_Best AUC score = 0.8149_** 

## Talks & Lectures
- AI Risks & Risk Management - Multiplier event for the REGULAITE project, Arcos de Valdevez, Portugal, February 2024
- Analysis of feedback from pilot test's questionnaires - Multiplier event for the REGULAITE project, Arcos de Valdevez, Portugal, February 2024

## Publications
1. Castane, G.G., Martinez, A., Radaman, Q., Gkika, Z., Panagiotis, M., Vyhmeister, E. Market Analysis of a Data Platform in the European Data Ecosystem (2024). DOI: 10.1007/978-3-031-63227-3__7
