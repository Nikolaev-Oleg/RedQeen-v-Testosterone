import numpy as np
import os
import pandas as pd
from ultralytics import YOLO
import torch
import torchvision

row_TP = [int]
row_FP = [int]
row_FN = [int]
row_conf = [int]

steps = [x / 100 for x in range(0, 101, 1)]
for confidence in steps:
    TP = 0
    FP = 0
    FN = 0

    for name in os.listdir('./data/labels/val'):
        path_to_val = f'./data/labels/val/{name}'
        path_to_img = path_to_val.replace('labels', 'images')
        path_to_img = os.path.splitext(path_to_img)[0]
        path_to_img = f"{path_to_img}.jpg"

        # Validation data
        val = []

        with open(path_to_val, 'r') as file:
            lines = file.readlines()

        for line in lines:
            line = line.strip()
            if line:
                row = line.split(' ')
                cl = row[0]
                row = row[1:]
                row = [float(item) for item in row]
                if cl in ['0', '12', '17']:
                    val.append(row)

        val = np.array(val)

        x1 = val[:, 0] - val[:, 2] / 2
        y1 = val[:, 1] - val[:, 3] / 2
        x2 = val[:, 0] + val[:, 2] / 2
        y2 = val[:, 1] + val[:, 3] / 2

        val = np.stack([x1, y1, x2, y2], axis=1)
        val = torch.from_numpy(val).float()

        # Prediction
        cl = [0, 12, 17]  # RBC, imRBC, lysRBC
        model = YOLO('../runs_Egor/runs/detect/train5/weights/best.pt')

        predict = model.predict(path_to_img,
                                save=False,
                                classes=cl,
                                agnostic_nms=True,
                                # show_labels=False,
                                show_conf=False,
                                line_width=2,
                                conf=confidence)
        predict = predict[0].boxes.xyxyn
        predict = np.array(predict)
        predict = torch.from_numpy(predict).float()

        # Get matrix
        threshold = 0.6
        out = torchvision.ops.box_iou(val, predict)
        out = np.array(out)
        out[out < threshold] = 0

        result = np.zeros_like(out, dtype=int)
        for i in range(out.shape[0]):
            for j in range(out.shape[1]):
                value = out[i, j]
                if value != 0:
                    row_max = np.max(out[i, :])
                    col_max = np.max(out[:, j])
                    is_row_max = (value == row_max)
                    is_col_max = (value == col_max)
                    if is_row_max and is_col_max:
                        result[i, j] = 1
                    else:
                        result[i, j] = 0
        result = pd.DataFrame(result)

        tp = len(result.columns[(result == 1).any()])
        fp = result.shape[1] - tp
        fn = result.shape[0] - tp

        TP = TP + tp
        FP = FP + fp
        FN = FN + fn

    print('TP ' + str(TP))
    print('FP ' + str(FP))
    print('FN ' + str(FN))

    row_TP.append(TP)
    row_FP.append(FP)
    row_FN.append(FN)
    row_conf.append(confidence)

final_df = pd.DataFrame({'tp': row_TP,
                         'fp': row_FP,
                         'fn': row_FN,
                         'conf': row_conf})
print(final_df)
final_df.to_csv('metrics_out.csv')
