/*
BSol Registered Population Under 4 by LSOA21
*/

SELECT 
	LSOA_2021,
	COUNT(*) AS Observations
FROM EAT_Reporting_BSOL.Demographic.BSOL_Registered_Population 
WHERE
	ProxyAgeAtEOM < 4
GROUP BY 
	LSOA_2021

