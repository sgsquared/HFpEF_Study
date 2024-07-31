
-- Joining all output tables into master table - need to deduce which columns I need and then join all on ADMID - the only table I won't be able to join is the one on number of subsequent admissions

USE heart_failure_outputs;

SELECT * FROM patientdetails_output_202406141717;

SELECT
  patientdetails_output.ADMID,
  patientdetails_output.PATIENTID,
  patientdetails_output.PROB_NAME AS HFPEF_DIAG,
  patientdetails_output.AGE_GRP_AT_ADM AS AGE_ADMISSION,
  patientdetails_output.GENDER,
  patientdetails_output.PCODE_AREA AS POSTCODE,
  patientdetails_output.ETHNICITY,
  patientdetails_output.ADM_SOURCE,
  patientdetails_output.DISCH_DEST,
  patientdetails_output.TFC_DESC AS SPECIALTY,
  CONCAT(patientdetails_output.ADM_DATE, ' ', patientdetails_output.ADM_TIME) AS ADM_DATETIME,
  CONCAT(patientdetails_output.DIS_DATE, ' ', patientdetails_output.DIS_TIME) AS DISCH_DATETIME,
  demographics_output.AF,
  demographics_output.CAD,
  demographics_output.Hypertension,
  demographics_output.Heart_Failure,
  demographics_output.Valvular_Heart_Disease,
  demographics_output.Pacemaker,
  demographics_output.CVA,
  demographics_output.PVD,
  demographics_output.Anaemia,
  demographics_output.Asthma,
  demographics_output.COPD,
  demographics_output.Smoker,
  demographics_output.OSA,
  demographics_output.OHS,
  demographics_output.Obesity,
  demographics_output.Diabetes,
  demographics_output.CKD,
  demographics_output.Inflammatory_Bowel_Disease,
  demographics_output.Thyroid_Disease,
  demographics_output.Rheumatological_Disease,
  demographics_output.VTE,
  demographics_output.Malignancy,
  demographics_output.Mental_Health,
  medication_output.ACEi,
  medication_output.ARB,
  medication_output.MRA,
  medication_output.SGLT2i,
  medication_output.SacubitrilValsartan,
  medication_output.Thiazide,
  medication_output.LoopDiuretic,
  medication_output.PotassiumSparing AS Amiloride,
  medication_output.BBlocker,
  medication_output.Statin,
  medication_output.Ivabradine,
  medication_output.Hydralazine,
  medication_output.Nitrates,
  admissionobservations_output.O2SAT,
  admissionobservations_output.RR,
  admissionobservations_output.SBP,
  admissionobservations_output.DBP,
  admissionobservations_output.HR,
  admissionobservations_output.Weight,
  admissionobservations_output.Height,
  admissionbloods_output.WCC,
  admissionbloods_output.Hb,
  admissionbloods_output.Plts,
  admissionbloods_output.Na,
  admissionbloods_output.K,
  admissionbloods_output.Cr,
  admissionbloods_output.Ur,
  admissionbloods_output.CRP,
  admissionbloods_output.Alb,
  admissionbloods_output.INR,
  admissionbloods_output.Glucose,
  admissionbloods_output.Troponin,
  admissionbloods_output.Lactate,
  bnp_output.BNP,
  bnp_output.RESULT_DATETIME AS BNP_DATETIME,
  icuadmission_output.ITU_ADMDATETIME,
  icuadmission_output.ITU_DISCHDATETIME,
  icuadmission_output.DURATION_OF_ITU,
  admissionrr_output.RR AS ADM_RR,
  admissionrr_output.REC_DATETIME AS ADM_RR_DATETIME,
  24hourrr_output.RR24HR AS 24HR_RR,
  24hourrr_output.REC_DATETIME AS 24HR_RR_DATETIME,
  dischargerr_output.RRDisch AS DISCH_RR,
  dischargerr_output.REC_DATETIME AS DISCH_RR_DATETIME,
  admissionweight_output.Weight AS ADM_WEIGHT,
  admissionweight_output.REC_DATETIME AS ADM_WEIGHT_DATETIME,
  24hourweight_output.Weight24HR AS 24HR_WEIGHT,
  24hourweight_output.REC_DATETIME AS 24HR_WEIGHT_DATETIME,
  dischargeweight_output.WeightDisch AS DISCH_WEIGHT,
  dischargeweight_output.REC_DATETIME AS DISCH_WEIGHT_DATETIME,
  fluidbalance_output.TOTAL_INPUT,
  fluidbalance_output.TOTAL_OUTPUT,
  fluidbalance_output.FLUID_BALANCE,
  timeoutofhosp_output.TIME_TO_NEXT,
  admissionsinayear_output.NUMBER_OF_ADMISSIONS_IN_1YR,
  admissionsinayear_output.ADMISSION_NUMBER
FROM
  patientdetails_output
  LEFT JOIN demographics_output ON patientdetails_output.ADMID = demographics_output.ADMID
  LEFT JOIN medication_output ON patientdetails_output.ADMID = medication_output.ADMID
  LEFT JOIN admissionobservations_output ON patientdetails_output.ADMID = admissionobservations_output.ADMID
  LEFT JOIN admissionbloods_output ON patientdetails_output.ADMID = admissionbloods_output.ADMID
  LEFT JOIN bnp_output ON patientdetails_output.ADMID = bnp_output.ADMID
  LEFT JOIN icuadmission_output ON patientdetails_output.ADMID = icuadmission_output.ADMID
  LEFT JOIN admissionrr_output ON patientdetails_output.ADMID = admissionrr_output.ADMID
  LEFT JOIN 24hourrr_output ON patientdetails_output.ADMID = 24hourrr_output.ADMID
  LEFT JOIN dischargerr_output ON patientdetails_output.ADMID = dischargerr_output.ADMID
  LEFT JOIN admissionweight_output ON patientdetails_output.ADMID = admissionweight_output.ADMID
  LEFT JOIN 24hourweight_output ON patientdetails_output.ADMID = 24hourweight_output.ADMID
  LEFT JOIN dischargeweight_output ON patientdetails_output.ADMID = dischargeweight_output.ADMID
  LEFT JOIN fluidbalance_output ON patientdetails_output.ADMID = fluidbalance_output.ADMID
  LEFT JOIN timeoutofhosp_output ON patientdetails_output.ADMID = timeoutofhosp_output.ADMID
  LEFT JOIN admissionsinayear_output ON patientdetails_output.ADMID = admissionsinayear_output.ADMID;


