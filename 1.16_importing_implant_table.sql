# Importing the table named implant

use heart_failure;

CREATE TABLE implant (
    ADMID VARCHAR(64),
	PATIENTID VARCHAR(64),
    IMP_TYPE VARCHAR(25),
    IMP_NAME VARCHAR(150),
    IMPLANT_DATE DATE,
    AGE_GRP_AT_IMPLANT VARCHAR(10),
    EXPLANT_DATE DATE,
    AGE_GRP_AT_EXPLANT VARCHAR(10)
);

LOAD DATA LOCAL INFILE 'C:/ProgramData/MySQL/MySQL Server 8.0/Uploads/Implant.csv'
INTO TABLE heart_failure.implant
FIELDS TERMINATED BY ','
IGNORE 1 ROWS;

select COUNT(*) from implant;
