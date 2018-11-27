Final Project
===============

Jude Canady, Cameron Navero

Does the median income of an area affect the fix of road related issues in Baton Rouge?

Using the 311 data set, we filtered the collection of all 311 calls concerning road issues.
These were further sorted into their respective census tracts as designated by the US Census
Bureau. The median income of each tract was derived from an API provided by Data USA, which 
allows a user to easily query census-related data. A final table was made using these data 
sources which joined the median income, census tract, median tract fix time, and mean tract
fix time. From this, we were able to determine that no clear bias exists for selecting
which roads to improve based on the median income of that area. The correlation coefficients
between the median income of an area and the mean and median tract time are 0.28 and 0.30 
respectively. Scatter plots and heat maps were used to further confirm that the data was 
only slighlty correlated. 
