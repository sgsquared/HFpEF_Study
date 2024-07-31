SET GLOBAL local_infile=ON;

use heart_failure;

CREATE TABLE obs (
    ADMID VARCHAR(100),
	PATIENTID VARCHAR(64),
    REC_DATE INT,
    REC_TIME TIME,
    AGE_GRP_AT_REC VARCHAR(10),
    OBS_TYPE VARCHAR(32),
    REC DECIMAL
    );
    
LOAD DATA INFILE 'C:/ProgramData/MySQL/MySQL Server 8.0/Uploads/Obs.txt' IGNORE
INTO TABLE heart_failure.obs
FIELDS TERMINATED BY '	'
LINES TERMINATED BY '\n'
IGNORE 1 LINES;

# Converting excel date to normal date
UPDATE obs
SET REC_DATE = NULL
WHERE REC_DATE IS NULL
   OR REC_DATE = ''
   OR NOT REC_DATE REGEXP '^[0-9]+$';

ALTER TABLE obs
ADD COLUMN REC_DATE_N DATE;

UPDATE obs
SET REC_DATE_N = DATE_ADD('1899-12-30', INTERVAL REC_DATE DAY)
WHERE REC_DATE IS NOT NULL;