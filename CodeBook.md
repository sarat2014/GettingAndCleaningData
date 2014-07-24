CodeBook
========
This is the CodeBook for Tidy data `HARUsingSmartphones.txt` created using `run_analysis.R`.

Variable list and descriptions
------------------------------

Variable name    | Description
-----------------|------------
subject          | ID of the subject who performed the activity for each window sample. Its range is from 1 to 30.
activity         | Activity name
featDomain       | Feature: Time domain signal or frequency domain signal (Time or Freq)
featInstrument   | Feature: Measuring instrument (Accelerometer or Gyroscope)
featAcceleration | Feature: Acceleration signal (Body or Gravity)
featVariable     | Feature: Variable (Mean or SD)
featJerk         | Feature: Jerk signal
featMagnitude    | Feature: Magnitude of the signals calculated using the Euclidean norm
featAxis         | Feature: 3-axial signals in the X, Y and Z directions (X, Y, or Z)
featCount        | Feature: Count of data points used to compute `average`
featAverage      | Feature: Average of each variable for each activity and each subject

Structure of Dataset
-----------------

```r
tidyData <- read.table("HARUsingSmartphones.txt", sep="\t", header=TRUE)
str(tidyData)
```

```
'data.frame':	11880 obs. of  11 variables:
 $ subject         : int  1 1 1 1 1 1 1 1 1 1 ...
 $ activity        : Factor w/ 6 levels "LAYING","SITTING",..: 1 1 1 1 1 1 1 1 1 1 ...
 $ featDomain      : Factor w/ 2 levels "Freq","Time": 2 2 2 2 2 2 2 2 2 2 ...
 $ featAcceleration: Factor w/ 2 levels "Body","Gravity": NA NA NA NA NA NA NA NA NA NA ...
 $ featInstrument  : Factor w/ 2 levels "Accelerometer",..: 2 2 2 2 2 2 2 2 2 2 ...
 $ featJerk        : Factor w/ 1 level "Jerk": NA NA NA NA NA NA NA NA 1 1 ...
 $ featMagnitude   : Factor w/ 1 level "Magnitude": NA NA NA NA NA NA 1 1 NA NA ...
 $ featVariable    : Factor w/ 2 levels "Mean","SD": 1 1 1 2 2 2 1 2 1 1 ...
 $ featAxis        : Factor w/ 3 levels "X","Y","Z": 1 2 3 1 2 3 NA NA 1 2 ...
 $ count           : int  50 50 50 50 50 50 50 50 50 50 ...
 $ average         : num  -0.0166 -0.0645 0.1487 -0.8735 -0.9511 ...
```


A sample of the dataset
------------------------------


```r
head(tidyData, n=3)
```

```
  subject activity featDomain featAcceleration featInstrument featJerk featMagnitude featVariable featAxis count     average
1       1   LAYING       Time             <NA>      Gyroscope     <NA>          <NA>         Mean        X    50 -0.01655309
2       1   LAYING       Time             <NA>      Gyroscope     <NA>          <NA>         Mean        Y    50 -0.06448612
3       1   LAYING       Time             <NA>      Gyroscope     <NA>          <NA>         Mean        Z    50  0.14868944
```


Summary of variables
--------------------

```r
summary(tidyData)
```

```
    subject                   activity    featDomain  featAcceleration       featInstrument featJerk      featMagnitude  featVariable featAxis        count       average        
 Min.   : 1.0   LAYING            :1980   Freq:4680   Body   :5760     Accelerometer:7200   Jerk:4680   Magnitude:3240   Mean:5940    X   :2880   Min.   :36.00  -0.99767  
 1st Qu.: 8.0   SITTING           :1980   Time:7200   Gravity:1440     Gyroscope    :4680   NA's:7200   NA's     :8640   SD  :5940    Y   :2880   1st Qu.:49.00  -0.96205  
 Median :15.5   STANDING          :1980               NA's   :4680                                                                    Z   :2880   Median :54.50  -0.46989  
 Mean   :15.5   WALKING           :1980                                                                                               NA's:3240   Mean   :57.22  -0.48436  
 3rd Qu.:23.0   WALKING_DOWNSTAIRS:1980                                                                                                           3rd Qu.:63.25  -0.07836  
 Max.   :30.0   WALKING_UPSTAIRS  :1980                                                                                                           Max.   :95.00   0.97451  
```


Possible combinations of features
---------------------------------

```
   featDomain featAcceleration featInstrument featJerk featMagnitude featVariable featAxis   N
 1       Time               NA      Gyroscope       NA            NA         Mean        X 180
 2       Time               NA      Gyroscope       NA            NA         Mean        Y 180
 3       Time               NA      Gyroscope       NA            NA         Mean        Z 180
 4       Time               NA      Gyroscope       NA            NA           SD        X 180
 5       Time               NA      Gyroscope       NA            NA           SD        Y 180
 6       Time               NA      Gyroscope       NA            NA           SD        Z 180
 7       Time               NA      Gyroscope       NA     Magnitude         Mean       NA 180
 8       Time               NA      Gyroscope       NA     Magnitude           SD       NA 180
 9       Time               NA      Gyroscope     Jerk            NA         Mean        X 180
10       Time               NA      Gyroscope     Jerk            NA         Mean        Y 180
11       Time               NA      Gyroscope     Jerk            NA         Mean        Z 180
12       Time               NA      Gyroscope     Jerk            NA           SD        X 180
13       Time               NA      Gyroscope     Jerk            NA           SD        Y 180
14       Time               NA      Gyroscope     Jerk            NA           SD        Z 180
15       Time               NA      Gyroscope     Jerk     Magnitude         Mean       NA 180
16       Time               NA      Gyroscope     Jerk     Magnitude           SD       NA 180
17       Time             Body  Accelerometer       NA            NA         Mean        X 180
18       Time             Body  Accelerometer       NA            NA         Mean        Y 180
19       Time             Body  Accelerometer       NA            NA         Mean        Z 180
20       Time             Body  Accelerometer       NA            NA           SD        X 180
21       Time             Body  Accelerometer       NA            NA           SD        Y 180
22       Time             Body  Accelerometer       NA            NA           SD        Z 180
23       Time             Body  Accelerometer       NA     Magnitude         Mean       NA 180
24       Time             Body  Accelerometer       NA     Magnitude           SD       NA 180
25       Time             Body  Accelerometer     Jerk            NA         Mean        X 180
26       Time             Body  Accelerometer     Jerk            NA         Mean        Y 180
27       Time             Body  Accelerometer     Jerk            NA         Mean        Z 180
28       Time             Body  Accelerometer     Jerk            NA           SD        X 180
29       Time             Body  Accelerometer     Jerk            NA           SD        Y 180
30       Time             Body  Accelerometer     Jerk            NA           SD        Z 180
31       Time             Body  Accelerometer     Jerk     Magnitude         Mean       NA 180
32       Time             Body  Accelerometer     Jerk     Magnitude           SD       NA 180
33       Time          Gravity  Accelerometer       NA            NA         Mean        X 180
34       Time          Gravity  Accelerometer       NA            NA         Mean        Y 180
35       Time          Gravity  Accelerometer       NA            NA         Mean        Z 180
36       Time          Gravity  Accelerometer       NA            NA           SD        X 180
37       Time          Gravity  Accelerometer       NA            NA           SD        Y 180
38       Time          Gravity  Accelerometer       NA            NA           SD        Z 180
39       Time          Gravity  Accelerometer       NA     Magnitude         Mean       NA 180
40       Time          Gravity  Accelerometer       NA     Magnitude           SD       NA 180
41       Freq               NA      Gyroscope       NA            NA         Mean        X 180
42       Freq               NA      Gyroscope       NA            NA         Mean        Y 180
43       Freq               NA      Gyroscope       NA            NA         Mean        Z 180
44       Freq               NA      Gyroscope       NA            NA           SD        X 180
45       Freq               NA      Gyroscope       NA            NA           SD        Y 180
46       Freq               NA      Gyroscope       NA            NA           SD        Z 180
47       Freq               NA      Gyroscope       NA     Magnitude         Mean       NA 180
48       Freq               NA      Gyroscope       NA     Magnitude           SD       NA 180
49       Freq               NA      Gyroscope     Jerk     Magnitude         Mean       NA 180
50       Freq               NA      Gyroscope     Jerk     Magnitude           SD       NA 180
51       Freq             Body  Accelerometer       NA            NA         Mean        X 180
52       Freq             Body  Accelerometer       NA            NA         Mean        Y 180
53       Freq             Body  Accelerometer       NA            NA         Mean        Z 180
54       Freq             Body  Accelerometer       NA            NA           SD        X 180
55       Freq             Body  Accelerometer       NA            NA           SD        Y 180
56       Freq             Body  Accelerometer       NA            NA           SD        Z 180
57       Freq             Body  Accelerometer       NA     Magnitude         Mean       NA 180
58       Freq             Body  Accelerometer       NA     Magnitude           SD       NA 180
59       Freq             Body  Accelerometer     Jerk            NA         Mean        X 180
60       Freq             Body  Accelerometer     Jerk            NA         Mean        Y 180
61       Freq             Body  Accelerometer     Jerk            NA         Mean        Z 180
62       Freq             Body  Accelerometer     Jerk            NA           SD        X 180
63       Freq             Body  Accelerometer     Jerk            NA           SD        Y 180
64       Freq             Body  Accelerometer     Jerk            NA           SD        Z 180
65       Freq             Body  Accelerometer     Jerk     Magnitude         Mean       NA 180
66       Freq             Body  Accelerometer     Jerk     Magnitude           SD       NA 180
   featDomain featAcceleration featInstrument featJerk featMagnitude featVariable featAxis   N
```
