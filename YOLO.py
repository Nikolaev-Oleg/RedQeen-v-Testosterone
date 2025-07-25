import numpy as np
import os
import cv2
from ultralytics import YOLO

model = YOLO("yolov8s.yaml")
results = model.train(data='data.yaml', epochs=500, imgsz=640, plots=True, save_period=10)

model = YOLO('../runs_Egor/runs/detect/train5/weights/best.pt')
results = model.train(resume=True)

predict = model.predict('C:/Users/nikol/OneDrive/Рабочий стол/Статьи в работе/Haemogregarina/Darevskia_blood/aside_of_model/SmearsForCounting/AC185_Dport_m_130923', save=True)

s = 0
for i in predict:
    s = s + len(i.boxes)
print(s)
