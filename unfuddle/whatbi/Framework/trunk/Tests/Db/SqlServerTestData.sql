IF NOT EXISTS (SELECT * FROM sysdatabases WHERE NAME='WhatbiTest')
BEGIN
	CREATE DATABASE WhatbiTest
END
GO

USE WhatbiTest
GO

IF EXISTS (SELECT * FROM INFORMATION_SCHEMA.TABLES WHERE TABLE_NAME='Cavemen')
BEGIN
	TRUNCATE TABLE Cavemen
END
ELSE
BEGIN
	CREATE TABLE Cavemen
	(
	-- Columns
		[Name]	VARCHAR(64) NOT NULL
	,	Job	VARCHAR(64)
	-- Constraints
	PRIMARY KEY ([Name])
	)
END
GO

INSERT INTO Cavemen([Name], Job) VALUES ('Fred', 'Quarryman')
GO

INSERT INTO Cavemen([Name], Job) VALUES ('Wilma', 'Cavewife')
GO

INSERT INTO Cavemen([Name], Job) VALUES ('Barney', 'Quarryman')
GO

INSERT INTO Cavemen([Name], Job) VALUES ('Betty', 'Cavewife')
GO
