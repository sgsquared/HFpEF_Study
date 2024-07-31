# Importing the table named thtr

SET GLOBAL local_infile=ON;

use heart_failure;

CREATE TABLE thtr (
    ADMID VARCHAR(64),
    PATIENTID VARCHAR(64),
    OPCS_CODE VARCHAR(10),
    OPCS_DESC VARCHAR(100),
    OP_STRT_DATE DATE,
    OP_STRT_TIME TIME,
    AGE_GRP_AT_OP_STRT VARCHAR(10),
    OP_END_DATE DATE,
    OP_END_TIME TIME,
    AGE_GRP_AT_OP_END VARCHAR(10)
    );

LOAD DATA LOCAL INFILE 'C:/ProgramData/MySQL/MySQL Server 8.0/Uploads/Thtr.csv'
INTO TABLE heart_failure.thtr
FIELDS TERMINATED BY ','
IGNORE 1 ROWS;

select COUNT(*) from thtr;

DELETE FROM thtr
