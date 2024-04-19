%% APCP.m

%matlab version of USDA automated precip correction program

%Jenn Fair, modified by Kevin Ryan for hobo water level logger
% July 2017, April 2019, SEPT 2020, nov. 2021, Mar. 2023

clear all
close all
warning on
format long

%ADD path to access the correctPrecipitation.m function file
addpath('C:\Users\karyan\OneDrive - DOI\2 ANALYSIS\Prerana Bhaumik\Prerana\first_repo\matlab\');

%ADD precip data
W9Precipmm = readtable("C:\Users\karyan\OneDrive - DOI\2 ANALYSIS\Prerana Bhaumik\Prerana\first_repo\data\hobo\forMatlab\SF-A\hobo_SF-A_ml.csv")

DateString = W9Precipmm(:,1);
DateStringArray = table2array(DateString);
formatIn = 'yyyy-mm-dd HH:MM:SS';
date = datenum(DateStringArray);
%date = datetime(DateStringArray,formatIn);

W9PrecipmmArray = table2array(W9Precipmm(:,2));



CumPPT=[W9PrecipmmArray]; %create a table of the ppt

noData=[(-99*25.4)]; %data must not have gaps; set to high value (500) to
% exclude data from program calculations for bad or corrected data intervals
bucketDump=[0.001]; % in mm, any drop greater than decant value is considered
%a bucket dump.  This value should be greater than the noise value.
recharge=[0.5]; % in mm  flags increases due to oil/anitfreeze after dump
%Any value increase per time step less than recharge value is precip.
noise=[0.01]; 


%SOURCE THE correctPrecipitation.m function:
[precip_corr,FinalTime] = correctPrecipitation(date,CumPPT,bucketDump,recharge,noise,noData);

%figure(1)
%plot(date,CumPPT,'bo')
%legend('hobo raw')
%datetick('x','mm-dd-yyyy HH','keeplimits')
%ylabel('Raw PPT, mm/hour')

figure(2)
plot(FinalTime,precip_corr, 'bo')
legend('hobo corr')
ylabel('Corrected Precipitation, mm/hour')
datetick('x','mm-dd-yyyy','keeplimits')

%FinalTime2 = datestr(FinalTime);
%save output
W9PPTmmfinal = [FinalTime, precip_corr];

%writematrix(W9PPTmmfinal, 'C:\Users\KEVIN\Google Drive\SCHOOL\1 - USGS Sleepers VT\Analysis\scan-tree\data\hobo\forMatlab\hoboPPTcorr.csv', 'delimiter', ',')
writematix(W9PPTmmfinal, 'C:\Users\karyan\OneDrive - DOI\2 ANALYSIS\Prerana Bhaumik\Prerana\first_repo\data\hobo\forMatlab\hobo_corr_SFA.csv')
%dlmwrite('W9PPT.csv', W9PPTmmfinal, 'delimiter', ',');

