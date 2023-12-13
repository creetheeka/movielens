A movie recommendation system is a tool to suggest movies to users based on the ratings provided by the user. The tool employs various machine learning techniques to process and analyze data including user-provided data. It generates personalized movie recommendations based on user ratings. Based on a user rating, a one-star rating means it is not a good movie whereas a five-star rating means it is a great movie. Based on the ratings by the user, the recommendation system such as one used by Netflix, predicts how many stars a user will give to a movie. The famous Netflix challenge back in 2006, offered $1M USD for the best model to predict user ratings based on previous ratings of the user.

The goal of this project is to create a movie recommendation system using the MovieLens dataset containing 10 million movie ratings and by employing various tools learned throughout the courses in the PH125.x data science series. The MovieLens data and the code to generate the datasets are already provided to begin with. The objective is to determine a model that predicts ratings with a RMSE (Root Mean Square Error) less than 0.86490 versus the actual ratings in the final holdout set. 

In this report, several key steps are followed to achieve the objective by exploratory analysis of data including data visualization, and train and test the model using the datasets provided. The pre-processing techniques are applied to both edx and final holdout sets. The model is trained on the train set (edx_train_set) which is 90% of the edx dataset. The test set (edx_test_set) is derived from the 10% of the edx dataset. The final holdout set (final_holdout_test) is derived 10% from the original MovieLens data. The RMSE results are analyzed at each step and eventually the final model is validated by determining the RMSE on the final hold-out set. Please note that the final model is derived by training and testing on the edx dataset which consists of 90% of the original MovieLens data. The final holdout set consists of 10% of the original MovieLens data and is used **only** for the final validation against the final model to determine the RMSE and not for training or regularization.

RStudio 2023.09.1+494 "Desert Sunflower" Release (cd7011dce393115d3a7c3db799dda4b1c7e88711, 2023-10-16) for windows Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) RStudio/2023.09.1+494 Chrome/116.0.5845.190 Electron/26.2.4 Safari/537.36

R version info: platform x86_64-w64-mingw32
arch x86_64
os mingw32
crt ucrt
system x86_64, mingw32
status
major 4
minor 3.2
year 2023
month 10
day 31
svn rev 85441
language R
version.string R version 4.3.2 (2023-10-31 ucrt) nickname Eye Holes
