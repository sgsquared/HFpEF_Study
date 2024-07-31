-- Medication prior to or within 3 days of admission

USE heart_failure;

SELECT 
  MEDICATION2.ADMID,
  MAX(MEDICATION2.ACEi) AS ACEi,
  MAX(MEDICATION2.ARB) AS ARB,
  MAX(MEDICATION2.BBlocker) AS BBlocker,
  MAX(MEDICATION2.SGLT2i) AS SGLT2i,
  MAX(MEDICATION2.SacubitrilValsartan) AS SacubitrilValsartan,
  MAX(MEDICATION2.MRA) AS MRA,
  MAX(MEDICATION2.Thiazide) AS Thiazide,
  MAX(MEDICATION2.LoopDiuretic) AS LoopDiuretic,
  MAX(MEDICATION2.Statin) AS Statin,
  MAX(MEDICATION2.PotassiumSparing) AS PotassiumSparing,
  MAX(MEDICATION2.Ivabradine) AS Ivabradine,
  MAX(MEDICATION2.Hydralazine) AS Hydralazine,
  MAX(MEDICATION2.Nitrates) AS Nitrates
FROM (
  SELECT 
    MEDICATION.ADMID,
    CASE WHEN MEDICATION.MEDICATION LIKE '%pril%' THEN 'Y' END ACEi,
    CASE WHEN MEDICATION.MEDICATION LIKE '%artan%' THEN 'Y' END ARB,
    CASE WHEN MEDICATION.MEDICATION LIKE '%lol%' THEN 'Y' END BBlocker,
    CASE WHEN MEDICATION.MEDICATION LIKE '%gliflozin%' THEN 'Y' END SGLT2i,
    CASE WHEN MEDICATION.MEDICATION LIKE '%sacubitril%' THEN 'Y' END SacubitrilValsartan,
    CASE WHEN MEDICATION.MEDICATION LIKE '%eplerenone%' OR MEDICATION.MEDICATION LIKE '%spironolactone%' THEN 'Y' END MRA,
    CASE WHEN MEDICATION.MEDICATION LIKE '%metolazone%' OR MEDICATION.MEDICATION LIKE '%indapamide%' OR MEDICATION.MEDICATION LIKE '%thiazide%' THEN 'Y' END Thiazide,
    CASE WHEN MEDICATION.MEDICATION LIKE '%furosemide%' OR MEDICATION.MEDICATION LIKE '%bumetanide%' OR MEDICATION.MEDICATION LIKE '%torsemide%' THEN 'Y' END LoopDiuretic,
    CASE WHEN MEDICATION.MEDICATION LIKE '%statin%' THEN 'Y' END Statin,
    CASE WHEN MEDICATION.MEDICATION LIKE '%amiloride%' THEN 'Y' END PotassiumSparing,
    CASE WHEN MEDICATION.MEDICATION LIKE '%ivabradine%' THEN 'Y' END Ivabradine,
    CASE WHEN MEDICATION.MEDICATION LIKE '%hydralazine%' THEN 'Y' END Hydralazine,
    CASE WHEN MEDICATION.MEDICATION LIKE '%nitrate%' THEN 'Y' END Nitrates
  FROM (
    SELECT
    	probs.ADMID,
      CONCAT(adm.ADM_DATE, ' ', adm.ADM_TIME) AS ADMISSION_DATETIME,
      CONCAT(meds.SCHED_START_DATE, ' ', meds.SCHED_START_TIME) AS MED_DATETIME,
      meds.MEDICATION
    FROM
    	probs
      LEFT JOIN adm ON probs.ADMID = adm.ADMID
      LEFT JOIN meds ON probs.PATIENTID = meds.PATIENTID
    WHERE 
    	(probs.PROB_NAME LIKE '%HFpEF%'
    	OR probs.PROB_NAME LIKE '%preserved%'
    	OR probs.PROB_NAME LIKE '%diastolic%'
    	OR probs.PROB_NAME LIKE '%normal ejection%'
    	OR probs.PROB_NAME LIKE '%DHF%')
    AND
    	(((meds.MEDICATION LIKE '%pril%' OR
        meds.MEDICATION LIKE '%artan%' OR
        meds.MEDICATION LIKE '%lol%' OR
        meds.MEDICATION LIKE '%glifozin%' OR
        meds.MEDICATION LIKE '%digoxin%' OR
        meds.MEDICATION LIKE '%sacubitril%' OR
        meds.MEDICATION LIKE '%eplerenone%' OR
        meds.MEDICATION LIKE '%spironolactone%' OR
        meds.MEDICATION LIKE '%metolazone%' OR
        meds.MEDICATION LIKE '%furosemide%' OR
        meds.MEDICATION LIKE '%indapamide%' OR 
        meds.MEDICATION LIKE '%thiazide%' OR
        meds.MEDICATION LIKE '%statin%' OR
        meds.MEDICATION LIKE '%amiloride%' OR
        meds.MEDICATION LIKE '%bumetanide%' OR
        meds.MEDICATION LIKE '%torsemide%' OR
        meds.MEDICATION LIKE '%ivabradine%' OR
        meds.MEDICATION LIKE '%hydralazine%' OR
        meds.MEDICATION LIKE '%nitrate%') AND
        meds.MEDICATION NOT LIKE '%eye%') AND
        meds.MEDICATION NOT LIKE '%nystatin%')
    AND
      (CONCAT(meds.SCHED_START_DATE, ' ', meds.SCHED_START_TIME) <= DATE_ADD(CONCAT(adm.ADM_DATE, ' ', adm.ADM_TIME), INTERVAL 3 DAY))) AS MEDICATION
  ) AS MEDICATION2
  GROUP BY 
    MEDICATION2.ADMID