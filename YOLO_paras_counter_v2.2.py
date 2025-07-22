import numpy as np
import os
import cv2
from ultralytics import YOLO
import pandas as pd
import shutil

model = YOLO("C:/Users/nikol/OneDrive/Рабочий стол/Статьи в работе/Haemogregarina/YOLO2/runs/detect/train23/weights/last.pt")
root_dir = 'C:/Users/nikol/OneDrive/Изображения/Пленка/new1'

dirs = [f'{root_dir}/{d.name}' for d in os.scandir(root_dir) if d.is_dir()]

paras_df = pd.DataFrame(columns=['id', 'paras'])
for d in dirs:
    nfiles = len([f for f in os.listdir(d)])
    if nfiles > 0:
        results = model.predict(d,
                                save=True,
                                classes=0,
                                agnostic_nms=True,
                                project= root_dir,
                                name='temp',
                                conf=0.5)
        if os.path.exists(f'{d}/YOLO2.23l'):
            shutil.rmtree(f'{d}/YOLO2.23l')
        os.rename(f'{root_dir}/temp', f'{d}/YOLO2.23l')

        s = 0
        for i in results:
            s = s + len(i.boxes.cls[i.boxes.cls == 0])
        print('Paras:' + str(s)) # count T0

        name = d.replace(f'{root_dir}/', '')
        newrow = {'id': name, 'paras': s}

        paras_df.loc[len(paras_df)] = newrow
        paras_df.to_csv('./YOLO2.23l_paras_main2.csv',
                          index=False)
