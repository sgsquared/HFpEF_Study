use heart_failure;

CREATE TABLE cancer (
    PATIENTID VARCHAR(64),
    CANCER VARCHAR(2)
);

LOAD DATA LOCAL INFILE 'C:/ProgramData/MySQL/MySQL Server 8.0/Uploads/Cancer.txt'
INTO TABLE heart_failure.cancer
FIELDS TERMINATED BY '	'
LINES TERMINATED BY '\n'
IGNORE 1 ROWS;

select * from cancer;