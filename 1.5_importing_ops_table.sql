use heart_failure;

CREATE TABLE OPs (
	ADMID VARCHAR(64),
    PATIENTID VARCHAR(64),
    OPCS_CODE VARCHAR(6),
    OPCS_DESC VARCHAR(150),
    OP_DATE DATE,
    OP_TIME TIME,
    AGE_GRP_AT_OP VARCHAR(10)
);

LOAD DATA LOCAL INFILE 'C:/ProgramData/MySQL/MySQL Server 8.0/Uploads/OPs.csv'
INTO TABLE heart_failure.ops
FIELDS TERMINATED BY ','
IGNORE 1 ROWS;

DELETE From ops;

select * from ops
WHERE ADMID = "F8A5E5D0B3B0C758017AF4D875995E4D979A42E471F5F4195DE5A1F4D658102B";