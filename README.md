# Does the Red Queen rule apply to rock lizards?

# Methods
Three parthenogenetic (_Darevskia armeniaca_, _D. dahli_ and _D. unisexualis_) and three bisexual (_D. portschinskii_, _D. valentini_ and _D. raddei nairensis_) species were included in the study. Animals were collected in five sympatric populations in Armenia (SH, SR, HR, AS and GG) in 2022 and 2023.
Both in 2022 and 2023 we collected blood smears from tail tips. They were air dried, transported unstained to the laboratory and stained according to Wright’s modification of Romanowski-Giemsa protocol (Wright, 1902). The smears were then studied with the Carl Zeiss Axiostar plus microscope under ×400 magnification. Micrographs were taken using the ToupCam SCMOS05100KPA digital camera.

In 2023, we collected blood serum samples as well to measure the concentration of sex steroids. Blood sampling was performed at three timepoints (May, June and September) in SR and AC, in May and September in HR, in June and September in SH and in May in GG. 100-150 μl of blood from a caudal vein was collected to 0.5 ml centrifuge tubes and centrifuged at 3000 g for 10’. Supernatant was collected into 2 ml sealed tubes and stored at -20℃ until transportation to the laboratory. At the laboratory the tubes were stored at -80℃ until ELISA. All samples were stored for not longer than 8 months.

We calculated parasitic load for each individual as a proportion of  blood cells (RBC), infected by haemogregarine gamontes to all RBC, including heterochromatic RBC. If the total number of RBC for an individual was less than 1000, it was excluded from the sample. Animals with no parasites detected were considered uninfected. Infection presence for each cohort (i.e. sex and species) was calculated as a proportion of infected animals in the given cohort.
We trained a YOLOv8m computer vision model to count healthy RBC and a YOLOv10s model to calculate infected RBC. The trained models are further referred to as YOLOrbc and YOLOinfected. We used ultralytics (https://github.com/ultralytics/ultralytics) python package to train and validate both models.
YOLOrbc was trained on 18027 objects from 168 micrographs of 13 individuals and validated on 2282 objects from 42 micrographs. YOLOinfected was trained on 1462 infected RBC from 1067 micrographs of 20 individuals and validated on 831 infected RBC from 605 micrographs. Image annotation was performed in CVAT (https://github.com/cvat-ai/cvat).

The prediction mode implemented in ultralytics allows adjusting model precision and recall via selection of the threshold object detection confidence level. In order to select an optimal threshold for each model, we calculated false positive, true positive and false negative detection rates on the validation datasets given threshold confidence level 0, 0.01 … 0.99, 1. Detection was considered true positive if the intersection-over-union (IoU) of the predicted binding box and the true binding box was greater than 0.6. The confidence level threshold maximizing F1 score for each model was selected. Predictions of the models were checked manually for hallucinations and adjusted if required. Cell counts returned by the model were checked manually.

We compared sex steroids concentration and parasitic load within each cohort and study site between timepoints using Dunn’s test with Holm’s p adjustment. If only two time points were available, Mann-Whitney test was used.
Aligned rank transform ANOVA and Tukey’s post-hoc test was used to compare progesterone concentration between species and parasitic load between cohorts within each study site, while treating timepoint as a random effect. If the only timepoint was available, Dunn’s test was used instead. 

# Implementation
This repository contains the following files and folders:

- **YOLO_RBC**: Data and code for YOLOrbc model
  - **YOLO_train.py**: Ultralytics code for model training. Pretrained model configuration file yolov8s.yaml is included in Ultralytics package
  - **best.pt**: Weights of the trained model
  - **YOLO_RBC_counter_v1.2.py**: Cell counter based on the trained model. Takes a directory with multiple subdirectories with images and returns _a)_ a .csv file with cell counts and _b)_ a directory with annotated images. By default, distinguishes normal RBC, heterochromatic RBC (hcRBC) and RBC in a process of cell lysis (lysRBC)
  - **metric_calculator.py**: Computes the model performance metrics based on validation dataset. In particular, was used for optimal confedence selection
  - **metrics_out.csv**: The output of metric_calculator.py
  - **data**
    - **images**
      - **val**: A directory with validation set of images (used by metric_calculator.py)
    - **labels**
      - **val**: A directory with validation set of annotations (used by metric_calculator.py)
- **YOLO_paras**: Data and code for YOLOinfected model
  - **YOLO_train.py**: Ultralytics code for model training. Pretrained model configuration file yolov8s.yaml is included in Ultralytics package
  - **best.pt**: Weights of the trained model
  - **YOLO_paras_counter_v2.2.py**: Cell counter based on the trained model. Takes a directory with multiple subdirectories with images and returns _a)_ a .csv file with cell counts and _b)_ a directory with annotated images. Counts infected RBC. Does not classify parasites.
  - **metric_calculator2.py**: Computes the model performance metrics based on validation dataset. In particular, was used for optimal confedence selection
  - **metrics_out2.csv**: The output of metric_calculator2.py
  - **data**
    - **images**
      - **val**: A directory with validation set of images (used by metric_calculator2.py). Note that only 50 images are in this repo
    - **labels**
      - **val**: A directory with validation set of annotations (used by metric_calculator2.py). Note that only 50 images are in this repo
- **data**: contains input filis for statistical analysis
  - **YOLO1.5b_RBC_main2.csv**: Helthy RBC counts
  - **YOLO2.23l_paras_main2.csv**: Infected RBC counts
  - **horm.scv**: ELISA results
  - **paras.csv**: Integrates data from YOLO1.5b_RBC_main2.csv and YOLO2.23l_paras_main2.csv. Outputed from RedQueen_dataprep.R
  - **metdata.csv**: Animals' metadata (body mass, SVL, sex, species, date and site of collection)
  - **main_check.csv**: The table containing all the data, including sex steroids concentration and manually checked cell counts
- **RedQueen_dataprep.R**: Integrates horm.csv, metadata.csv and cell counts into a single table
- **RedQueen_main.R**: Main statistical pipline. !!! Alpha version !!!
- **RedQueen_quality_metrics.R**: Quality metrics of YOLOrbc and YOLOinfected models (FP/TP/FN, F1 etc.)
- **README.md**: This readme 

# Results

# Example output of YOLOinfected
<img width="2592" height="1944" alt="WIN_20240601_12_35_17_Pro" src="https://github.com/user-attachments/assets/1ae660ab-ae62-4cb9-b188-47d6f927e789" />

# Example output of YOLOrbc
<img width="2592" height="1944" alt="WIN_20240601_12_35_17_Pro" src="https://github.com/user-attachments/assets/a6bbcec8-cb7a-4525-92ee-cd2b83eecf70" />

# Model quality metrics

- **YOLOrbc:** AUPRC 0.95
- **YOLOinfected:** AUPRC 0.94

<img width="1500" height="1500" alt="Fig1" src="https://github.com/user-attachments/assets/907e6798-6c6b-4137-81d2-183b64d17aa0" />
Performance metrics of YOLOrbc and YOLOinfected models: reciever-operator curves for both models (a), F1 ~ Confidence curves for YOLOrbc (b), and YOLOinfected (c). We manually checked counts of healthy (d) and infected (e) RBC returned by he models as well as the proportion of infected RBC (f), and found sufficient concordance between automated and manual image processing. Arrows indicate optimal confidence level, dashed lines indicate where the results of automated and manual counting are equal.

# Data availability
We cannot attach all the images and model weights to this repository due to great number and size of the files. However, we would be glad to share them by request:
nikolaevod2002@gmail.com (Oleg Nikolaev)

