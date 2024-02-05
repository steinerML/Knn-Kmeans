# Knn-Kmeans
 K-NN and K-means algorithms: A Comprehensive Analysis of 1980s American Market Car Data.

These are - in a nutshell - the main bullet points that have been approached:

+ **File reading (.CSV)**
+ **Check number of records**
+ **Prepare the file for KNN method**
+ **Set 'label' variable**
+ **Normalize file, excluding 'label' variable**
+ **Create normalize function**
+ **Calculate K number of neighbours**
+ **Train the model**
+ **Evaluate the model**
+ **Model improvements**
+ **Z-score**

![Source Image Sequence](general.gif)

## Contents :
Object detection and tracking has numerous applications in computer vision, thus I wanted to summarize the main challenges we face when approaching a detection and tracking app in the following table. As we give solutions to challenges via built-in functions, I have only included the main functions used and a brief description of what each one does.

| Function            |Action                                                                        |
|:--------------------|------------------------------------------------------------------------------|
|*_Object Detection_*||
|project.py           | Main app|
|**cv2.VideoCapture()**   |We create the capture object|
|**cv2.createBackgroundSubtractorMOG2()** | Object Detector (background subtractor through mask)|
|**object_detector.apply()**| Apply object detector both to frame and roi.|
|**cv2.findContours()**     |Extract coordinates from mask.|
|**cv2.drawContours()**    | Draw contours.|
|**cv2.contourArea()**|Calculate Area.|
|**roi = frame[x1 : x2, y1 : y2]**|Extract region of intrest (ROI)|
|**cv2.createBackgroundSubtractorMOG2(history,varThreshold)**   |Improve Detection via history & varThreshold.|
|**cv2.boundingRect(),cv2.rectangle()**|Draw rectangle based on contour.|
|**cv2.drawContours()**    | Draw contours.|
|**cv2.threshold(mask, colour1, colour2,cv2.THRESH_BINARY)**    | Apply threshold to mask.colour1 & colour 2 range 0-255 BGR.|
|*_Object Tracking_*||
|object_tracker.py           | Tracker class|
|```from object_tracker import *```    | Import tracker so we can load EuclideanDistTracker class.|
|**tracker = EuclideanDistTracker()**    | Create object tracker.|
|**tracker.update(detections)**    | Tracker update.|
|**detections = [ ]**    | Empty list to store object coordinates (x, y, w, h).|
|**cv2.putText()**    | Add text.|
|**cv2.rectangle()**    | Add rectangle.|
cv2.imshow("Frame", frame)
cv2.imshow("Mask", mask)
cv2.imshow("ROI", roi)
```
