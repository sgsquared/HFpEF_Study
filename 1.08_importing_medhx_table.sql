# Importing the table named medhx

SET GLOBAL local_infile=ON;

use heart_failure;

CREATE TABLE medhx (
    ADMID VARCHAR(64),
	PATIENTID VARCHAR(64),
    HX_DATE INT,
    AGE_GRP_AT_HX_DATE VARCHAR(10),
    PROBLEM VARCHAR(16),
    MED_HX_NAME VARCHAR(64)
    );

LOAD DATA LOCAL INFILE 'C:/ProgramData/MySQL/MySQL Server 8.0/Uploads/MedHX.txt'
INTO TABLE heart_failure.medhx
FIELDS TERMINATED BY '	'
LINES TERMINATED BY '\n'
IGNORE 1 ROWS;

# Converting excel date to normal date
UPDATE medhx
SET HX_DATE = NULL
WHERE HX_DATE IS NULL
   OR HX_DATE = ''
   OR NOT HX_DATE REGEXP '^[0-9]+$';

ALTER TABLE medhx
ADD COLUMN HX_DATEN DATE;

UPDATE medhx
SET HX_DATEN = DATE_ADD('1899-12-30', INTERVAL HX_DATE DAY)
WHERE HX_DATE IS NOT NULL;


select COUNT(*) from medhx;