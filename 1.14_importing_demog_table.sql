# Importing table named demog

use heart_failure;

CREATE TABLE demog (
    PATIENTID VARCHAR(64),
    GENDER INT,
    PCODE_AREA VARCHAR(6),
    ETHNICITY VARCHAR(2),
    DOD INT,
    AGE_GRP_AT_DEATH VARCHAR(10)
);

LOAD DATA LOCAL INFILE 'C:/ProgramData/MySQL/MySQL Server 8.0/Uploads/Demog.txt'
INTO TABLE heart_failure.demog
FIELDS TERMINATED BY '	'
LINES TERMINATED BY '\n'
IGNORE 1 ROWS;

# Converting excel date to normal date
UPDATE demog
SET DOD = NULL
WHERE DOD IS NULL
   OR DOD = ''
   OR NOT DOD REGEXP '^[0-9]+$';

ALTER TABLE demog
ADD COLUMN DODN DATE;

UPDATE demog
SET DODN = DATE_ADD('1899-12-30', INTERVAL DOD DAY)
WHERE DOD IS NOT NULL;

select * from demog;
