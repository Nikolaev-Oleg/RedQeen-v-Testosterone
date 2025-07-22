import numpy as np
import os
import cv2
from ultralytics import YOLO

model = YOLO("../runs/detect/train6/weights/last.pt")
results = model.train(data='data.yaml', epochs=500, imgsz=640, plots=True, save_period=10)
