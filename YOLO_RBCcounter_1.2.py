import numpy as np
import os
import cv2
from ultralytics import YOLO
import pandas as pd
import shutil

model = YOLO("./runs_Egor/runs/detect/train5/weights/best.pt")
root_dir = 'C:/Users/nikol/OneDrive/Изображения/Пленка/new1'
dirs = [f'{root_dir}/{d.name}' for d in os.scandir(root_dir) if d.is_dir()]
cl = [0, 12, 17]

paras_df = pd.DataFrame(columns=['id', 'RBC', 'hcRBC', 'lysRBC'])
for d in dirs:
    nfiles = len([f for f in os.listdir(d)])
    if nfiles > 0:
        results = model.predict(d,
                                save=True,
                                classes=cl,
                                agnostic_nms=True,
                                project=root_dir,
                                name='temp')
        if os.path.exists(f'{d}/YOLO1.5b'):
            shutil.rmtree(f'{d}/YOLO1.5b')
        os.rename(f'{root_dir}/temp', f'{d}/YOLO1.5b')

        s = 0
        for i in results:
            s = s + len(i.boxes.cls[i.boxes.cls == 0])
        print('RBC:' + str(s))

        s1 = 0
        for i in results:
            s1 = s1 + len(i.boxes.cls[i.boxes.cls == 12])
        print('hcRBC:' + str(s1))

        s2 = 0
        for i in results:
            s2 = s2 + len(i.boxes.cls[i.boxes.cls == 17])
        print('lysRBC:' + str(s2))

        name = d.replace(f'{root_dir}/', '')
        newrow = {'id': name, 'RBC': s, 'hcRBC': s1, 'lysRBC': s2}

        paras_df.loc[len(paras_df)] = newrow
        paras_df.to_csv('./YOLO1.5b_RBC_main2.csv',
                          index=False)