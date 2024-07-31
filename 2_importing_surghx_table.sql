SET GLOBAL local_infile=ON;

use heart_failure;

CREATE TABLE surghx (
    ADMID VARCHAR(64),
	PATIENTID VARCHAR(64),
    HX_DATE DATE,
    AGE_GRP_AT_HX_DATE VARCHAR(10),
    PROC_CODE VARCHAR(16),
    SURG_HX_NAME VARCHAR(64)
    );

LOAD DATA LOCAL INFILE 'C:/ProgramData/MySQL/MySQL Server 8.0/Uploads/SurgHX.csv'
INTO TABLE heart_failure.surghx
FIELDS TERMINATED BY ','
IGNORE 1 ROWS;

select COUNT(*) from surghx;
