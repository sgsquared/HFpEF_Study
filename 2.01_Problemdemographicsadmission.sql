# Isolating patient demographics and admission details

-- Problem, demographics, admission details
SELECT
	probs.PATIENTID,
	probs.ADMID,
    probs.PROB_NAME,
    probs.AGE_GRP_AT_NOTED_DATE,
    adm.AGE_GRP_AT_ADM,
    demog.GENDER,
    demog.PCODE_AREA,
    demog.ETHNICITY,
    adm.TFC_DESC,
    adm.ADM_SOURCE,
    adm.ADM_DATE,
    adm.ADM_TIME,
    adm.DISCH_DEST,
    adm.DIS_DATE,
    adm.DIS_TIME
FROM
	probs
    LEFT JOIN demog on probs.PATIENTID = demog.PATIENTID
    LEFT JOIN adm on probs.ADMID = adm.ADMID
WHERE 
	probs.PROB_NAME LIKE '%HFpEF%'
	OR probs.PROB_NAME LIKE '%preserved%'
	OR probs.PROB_NAME LIKE '%diastolic%'
	OR probs.PROB_NAME LIKE '%normal ejection%'
	OR probs.PROB_NAME LIKE '%DHF%';
