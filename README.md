# UNIVERSITY-OF-LIVERPOOL-ION-SWITCHING
Trained a model in R with WaveNet approach using multiple deep learning strategies from Computer Vision (CV)  and Audio Signal Processing models and applies them to longitudinal(time-series) data with the help of RccpRoll , Keras libraries where the training data contains 10 batches and testing data contains 4 batches ,and to predict the  number of open channels from signal at each time step.
Abstract:
The training data is recordings in time. At each 10,000th of a second, the strength of the signal 
was recorded and the number of ion channels open was recorded. It is our task to build a model 
that predicts the number of open channels from signal at each time step. Furthermore we are told 
that the data was recorded in batches of 50 seconds. Therefore each 500,000 rows is one batch. 
The training data contains 10 batches and the test data contains 4 batches. Let's display the number 
of open channels and signal strength together for each training batch.
