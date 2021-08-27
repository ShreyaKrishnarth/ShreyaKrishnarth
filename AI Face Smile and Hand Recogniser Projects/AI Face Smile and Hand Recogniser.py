#!/usr/bin/env python
# coding: utf-8

# In[7]:


pip install opencv-python


# In[2]:


pip install mediapipe


# In[4]:


pip install times


# In[2]:


conda install -c menpo opencv


# In[3]:


conda create --name myenv


# In[6]:


import cv2


# In[1]:


import cv2
face_detector = cv2.CascadeClassifier("haarcascade_frontalface_default.xml")
smile_detector = cv2.CascadeClassifier('haarcascade_smile.xml')

web = cv2.VideoCapture(0)
while True:
    successful_frame_read, frame = web.read()
    igray = cv2.cvtColor(frame,cv2.COLOR_BGR2GRAY)
    face_cordinate = face_detector.detectMultiScale(igray)
    smile_cordinate = smile_detector.detectMultiScale(igray,scaleFactor=1.7,minNeighbors=20)
    
    for(a,b,c,d) in face_cordinate:
        cv2.rectangle(frame,(a,b),(a+c, b+d), (0,255,0), 2)
        face = frame[b:b+d, a:a+c]
        
        igray = cv2.cvtColor(face,cv2.COLOR_BGR2GRAY)
        smile_cordinate = smile_detector.detectMultiScale(igray,scaleFactor=1.9,minNeighbors=25)
        
        #for(x,y,w,h) in smile_cordinate:
            #cv2.rectangle(face,(x,y),(x+w, y+h), (250,0,0), 2)
            
        if len(smile_cordinate) > 0:
            cv2.putText(frame, 'smiling', (a, b+d+40), fontScale=2,
            fontFace=cv2.FONT_HERSHEY_SCRIPT_COMPLEX, color=(0,0,255))
        
    cv2.imshow('smile detector',frame)
    key = cv2.waitKey(1)
    if key == 81 or key == 27:
        cv2.destroyAllWindows()
        break
web.release()


# In[ ]:


import cv2
import mediapipe as mp
import time 

cap = cv2.VideoCapture(0)

mpHands = mp.solutions.hands
hands = mpHands.Hands()
mpDraw = mp.solutions.drawing_utils

pTime = 0
cTime = 0


while True:
    success, img = cap.read()

    imgRGB = cv2.cvtColor(img, cv2.COLOR_BGR2RGB)
    result = hands.process(imgRGB)

    if result.multi_hand_landmarks:
        for handLms in result.multi_hand_landmarks:
            for Id, lm in enumerate(handLms.landmark):
                h, w, c = img.shape
                cx, cy = int(lm.x*w), int(lm.y*h)
                if Id == 8:
                    cv2.circle(img, (cx, cy), 15, (255, 0, 255), cv2.FILLED)

            mpDraw.draw_landmarks(img, handLms, mpHands.HAND_CONNECTIONS)

    cTime = time.time()
    fps = 1/(cTime-pTime)
    pTime = cTime

    cv2.putText(img, str(int(fps)), (10, 70), cv2.FONT_HERSHEY_PLAIN, 3, (255, 0, 255), 3)

    cv2.imshow("image", img)
    cv2.waitKey(1)


# In[ ]:




