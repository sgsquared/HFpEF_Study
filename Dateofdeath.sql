USE heart_failure;

SELECT 
  probs.ADMID,
  MAX(demog.DOD)
FROM
  probs
  LEFT JOIN adm ON probs.ADMID = adm.ADMID
  LEFT JOIN demog ON adm.PATIENTID = demog.PATIENTID
WHERE
  probs.PROB_NAME LIKE '%HFpEF%'
	OR probs.PROB_NAME LIKE '%preserved%'
	OR probs.PROB_NAME LIKE '%diastolic%'
	OR probs.PROB_NAME LIKE '%normal ejection%'
	OR probs.PROB_NAME LIKE '%DHF%'
GROUP BY
  probs.ADMID;
