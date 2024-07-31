use heart_failure;

CREATE TABLE fluid (
	ADMID VARCHAR(64),
    PATIENTID VARCHAR(64),
    FLUID_DATE INT,
    AGE_GRP_AT_FLUID_DATE VARCHAR(10),
    TOT_INPUT INT,
    TOT_OUTPUT INT
);

LOAD DATA LOCAL INFILE 'C:/ProgramData/MySQL/MySQL Server 8.0/Uploads/Fluid.txt'
INTO TABLE heart_failure.fluid
FIELDS TERMINATED BY '	'
LINES TERMINATED BY '\n'
IGNORE 1 ROWS;

# Converting excel date to normal date
UPDATE fluid
SET FLUID_DATE = NULL
WHERE FLUID_DATE IS NULL
   OR FLUID_DATE = ''
   OR NOT FLUID_DATE REGEXP '^[0-9]+$';

ALTER TABLE fluid
ADD COLUMN FLUID_DATEs DATE;

UPDATE fluid
SET FLUID_DATEs = DATE_ADD('1899-12-30', INTERVAL FLUID_DATE DAY)
WHERE FLUID_DATE IS NOT NULL;

select COUNT(*) from fluid;