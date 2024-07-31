-- SELECT COMORBITIES OF INTEREST DIAGNOSED PRIOR TO THE ADMISSION

  SELECT
    COMORBIDITIES2.ADMID,
    MAX(COMORBIDITIES2.AF) AS AF,
    MAX(COMORBIDITIES2.Anaemia) AS Anaemia,
    MAX(COMORBIDITIES2.Asthma) AS Asthma,
    MAX(COMORBIDITIES2.CAD) AS CAD,
    MAX(COMORBIDITIES2.Malignancy) AS Malignancy,
    MAX(COMORBIDITIES2.CKD) AS CKD,
    MAX(COMORBIDITIES2.COPD) AS COPD,
    MAX(COMORBIDITIES2.Diabetes) AS Diabetes,
    MAX(COMORBIDITIES2.Heart_Failure) AS Heart_Failure,
    MAX(COMORBIDITIES2.Hypertension) AS Hypertension,
    MAX(COMORBIDITIES2.Thyroid_Disease) AS Thyroid_Disease,
    MAX(COMORBIDITIES2.Inflammatory_Bowel_Disease) AS Inflammatory_Bowel_Disease,
    MAX(COMORBIDITIES2.Mental_Health) AS Mental_Health,
    MAX(COMORBIDITIES2.Obesity) AS Obesity,
    MAX(COMORBIDITIES2.OHS) AS OHS,
    MAX(COMORBIDITIES2.OSA) AS OSA,
    MAX(COMORBIDITIES2.Pacemaker) AS Pacemaker,
    MAX(COMORBIDITIES2.PVD) AS PVD,
    MAX(COMORBIDITIES2.Rheumatological_Disease) AS Rheumatological_Disease,
    MAX(COMORBIDITIES2.Smoker) AS Smoker,
    MAX(COMORBIDITIES2.CVA) AS CVA,
    MAX(COMORBIDITIES2.Valvular_Heart_Disease) AS Valvular_Heart_Disease,
    MAX(COMORBIDITIES2.VTE) AS VTE
  FROM (  
    SELECT
      COMORBIDITIES.ADMID,
      COMORBIDITIES.DIFF_DAYS,
      CASE WHEN COMORBIDITIES.MED_HX_NAME LIKE '%fibrillation%' THEN 'Y' END AF,
      CASE WHEN COMORBIDITIES.MED_HX_NAME LIKE '%anaemia%' THEN 'Y' END Anaemia,
      CASE WHEN COMORBIDITIES.MED_HX_NAME LIKE '%asthma%' THEN 'Y' END Asthma,
      CASE WHEN COMORBIDITIES.MED_HX_NAME LIKE '%angina%' OR '%coronary artery disease%' OR '%CABG%' OR '%ischaemic heart disease%' OR '%myocardial%' THEN 'Y' END CAD,
      CASE WHEN COMORBIDITIES.MED_HX_NAME LIKE '%lymphoma%' OR '%malignant%' OR '%cancer%' THEN 'Y' END Malignancy,
      CASE WHEN COMORBIDITIES.MED_HX_NAME LIKE '%chronic kidney disease%' OR '%chronic nephritis syndrome%' OR '%dialysis%' OR '%renal disease%' OR '%renal failure%' THEN 'Y' END CKD,
      CASE WHEN COMORBIDITIES.MED_HX_NAME LIKE '%chronic obstructive pulmonary%' OR '%emphysema%' THEN 'Y' END COPD,
      CASE WHEN COMORBIDITIES.MED_HX_NAME LIKE '%diabet%' THEN 'Y' END Diabetes,
      CASE WHEN COMORBIDITIES.MED_HX_NAME LIKE '%congestive%' OR '%heart failure%' OR '%left ventricular%' THEN 'Y' END Heart_Failure,
      CASE WHEN COMORBIDITIES.MED_HX_NAME LIKE '%hypertension%' THEN 'Y' END Hypertension,
      CASE WHEN COMORBIDITIES.MED_HX_NAME LIKE '%thyroid%' THEN 'Y' END Thyroid_Disease,
      CASE WHEN COMORBIDITIES.MED_HX_NAME LIKE '%crohn%' OR '%inflammatory bowel disease%' OR '%ulcerative colitis%' THEN 'Y' END Inflammatory_Bowel_Disease,
      CASE WHEN COMORBIDITIES.MED_HX_NAME LIKE '%anxiety%' OR '%depression%' OR '%mental%' OR '%self-harm%' THEN 'Y' END Mental_Health,
      CASE WHEN COMORBIDITIES.MED_HX_NAME LIKE 'obesity' THEN 'Y' END Obesity,
      CASE WHEN COMORBIDITIES.MED_HX_NAME LIKE 'obesity hypoventilation%' THEN 'Y' END OHS,
      CASE WHEN COMORBIDITIES.MED_HX_NAME LIKE 'sleep apnoea' OR 'sleep apnea' THEN 'Y' END OSA,
      CASE WHEN COMORBIDITIES.MED_HX_NAME LIKE '%pacemaker%' THEN 'Y' END Pacemaker,
      CASE WHEN COMORBIDITIES.MED_HX_NAME LIKE '%peripheral vascular disease%' THEN 'Y' END PVD,
      CASE WHEN COMORBIDITIES.MED_HX_NAME LIKE '%rheumatoid%' OR '%ankylosing%' OR '%polymyalgia%' THEN 'Y' END Rheumatological_Disease,
      CASE WHEN COMORBIDITIES.MED_HX_NAME LIKE '%smoker%' THEN 'Y' END Smoker,
      CASE WHEN COMORBIDITIES.MED_HX_NAME LIKE '%cerebrovascular accident%' OR '%transient ischaemic attack%' THEN 'Y' END CVA,
      CASE WHEN COMORBIDITIES.MED_HX_NAME LIKE '%nonrheumatic mitral valve%' OR '%aortic stenosis%' OR '%aortic valve%' OR '%heart valve%' THEN 'Y' END Valvular_Heart_Disease,
      CASE WHEN COMORBIDITIES.MED_HX_NAME LIKE '%DVT%' OR '%pulmonary embolism%' THEN 'Y' END VTE 
    FROM (  
      SELECT
        probs.ADMID,
        CONCAT(adm.ADM_DATE, ' ', adm.ADM_TIME) AS ADMISSION_DATETIME,
        medhx.HX_DATE,
        medhx.MED_HX_NAME,
        DATEDIFF(adm.ADM_DATE, medhx.HX_DATE) AS DIFF_DAYS
      FROM
        probs
        LEFT JOIN adm ON probs.ADMID = adm.ADMID
        LEFT JOIN medhx ON probs.PATIENTID = medhx.PATIENTID
      WHERE
        (probs.PROB_NAME LIKE '%HFpEF%'
      	OR probs.PROB_NAME LIKE '%preserved%'
      	OR probs.PROB_NAME LIKE '%diastolic%'
      	OR probs.PROB_NAME LIKE '%normal ejection%'
      	OR probs.PROB_NAME LIKE '%DHF%')
      	AND medhx.HX_DATE <= DATE_ADD(adm.ADM_DATE, INTERVAL 3 day)
      ) AS COMORBIDITIES
    ) AS COMORBIDITIES2
  GROUP BY 
    COMORBIDITIES2.ADMID;