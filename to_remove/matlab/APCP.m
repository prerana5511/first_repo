%% APCP.m

%matlab version of USDA automated precip correction program

%Jenn Fair
% July 2017

clear all
close all
warning off

load W9PPTraw16WY.mat
date=datenum(datetime(1:12:length(datetime)));
CumPPTw9=25.4*(PPTin(1:12:length(PPTin))); %convert to mm

load R1Apptraw16WY.mat
%date=datenum(DateTime);
%date=date(1:12:length(date));
CumPPTr1a=25.4.*(cumPPT(1:12:length(cumPPT))); % raw cumulative PPT converted to mm
%CumPPT=CumPPT(1:12:length(CumPPT));

% load R1Apptraw.mat
% CumPPTr1a=25.4.*cumPPT;  % raw cumulative PPT converted to mm

CumPPT=[CumPPTw9 CumPPTr1a]; %create a table of the two ppt recoords


noData=[(-99*25.4) (-99*25.4)];
bucketDump=[0.1 0.1]; % in mm
recharge=[25*12 25*12]; % in mm  why these numbers? Jenn?
noise=[0.001 0.001]; 

%[precip_corr,FinalTime] = correctPrecipitation(date,CumPPT);
[precip_corr,FinalTime] = correctPrecipitation(date,CumPPT,bucketDump,recharge,noise,noData);

W9apcp(1,:)=precip_corr(1,:);

[row,col]=size(precip_corr);

for i=2:row
    for    j=1:col;
    
        W9apcp(i,j)=precip_corr(i,j)-precip_corr(i-1,j); 
    
    end
    
end



figure(1)
plot(date,CumPPT(:,1),'bo',date,CumPPT(:,2),'ro')
legend('W9 raw','r1a raw')
datetick('x','mm-dd-yyyy HH','keeplimits')
ylabel('Raw PPT, mm/hour')

figure(2)
plot(FinalTime,W9apcp(:,1),'bo',FinalTime,W9apcp(:,2),'ro')
legend('W9 corr','r1a corr')
ylabel('Corrected Precipitation, mm/hour')
datetick('x','mm-dd-yyyy','keeplimits')

R29corr = W9apcp(:,1);
r1acorr = W9apcp(:,2);



n=24;
R29apcpdaily=sum(reshape(R29corr,n,366));
r1aapcpdaily=sum(reshape(r1acorr,n,366));
dailydt = FinalTime(1:n:length(FinalTime));


load W9dailyAnn.mat
Anndate=DATETIME;
AnnPPT=25.4.*INCHESPPT;
%AnnPPT=Pobsmmday;


% subset daily data

[start,enddate]=subset(dailydt,2015,10,1,2016,9,30);
dailydt=dailydt(start:enddate);
R29dailymm=R29apcpdaily(start:enddate);
r1adailymm=r1aapcpdaily(start:enddate);

[start,enddate]=subset(Anndate,2015,10,1,2016,9,30);
dailymmANN=AnnPPT(start:enddate);

figure(3)
plot(dailydt,dailymmANN,dailydt,R29dailymm,'LineWidth',2)
legend('Ann Daily Precip, mm','R29 APCP Daily Precip, mm')
datetick('x','mm-dd-yyyy','keeplimits')

figure(4)
plot(dailydt,dailymmANN,dailydt,r1adailymm,'LineWidth',2)
legend('Ann Daily Precip, mm','R1A APCP Daily Precip, mm')
datetick('x','mm-dd-yyyy','keeplimits')

% figure(4)
% scatter(dailymmAPCP,dailymmANN)
% xlabel('APCP daily precip, mm')
% ylabel('ANN daily precip, mm')

Er29=1-sum((dailymmANN'-R29dailymm).^2)./sum((dailymmANN-mean(dailymmANN)).^2)
Er1a=1-sum((dailymmANN'-r1adailymm).^2)./sum((dailymmANN-mean(dailymmANN)).^2)




for k=1:length(dailymmANN)
    
    a=abs(dailymmANN(k)-R29dailymm(k));
    b=abs(dailymmANN(k)-r1adailymm(k));
    
    if a>b
        finaldailymm(k)=r1adailymm(k);
        index(k)=1;
    else
        finaldailymm(k)=R29dailymm(k);
        index(k)=0;
    end
    
end

figure(5)
plot(dailydt,dailymmANN,dailydt,finaldailymm,'LineWidth',2)
legend('Ann Daily Precip, mm','final APCP Daily Precip, mm')
datetick('x','mm-dd-yyyy','keeplimits')

Efinal=1-sum((dailymmANN'-finaldailymm).^2)./sum((dailymmANN-mean(dailymmANN)).^2)

index=repelem(index,24);

k=find(index); %nonzero elements
l=find(index<1);

finalhourlymm=zeros(length(FinalTime),1);
finalhourlymm(k)=r1acorr(k);
finalhourlymm(l)=R29corr(l);

ix1=find(FinalTime==datenum(2016,7,22,00,00,00));
ix2=find(FinalTime==datenum(2016,7,26,00,00,00));
finalhourly(ix1:ix2)=R29corr(ix1:ix2);

ix3=find(dailydt==datenum(2016,7,21));
ix4=find(dailydt==datenum(2016,7,26));

figure(6)
plot(FinalTime,finalhourlymm)
datetick('x','mm-dd-yyyy','keeplimits')



%save('W9PPTcor2016WY.mat','FinalTime','finalhourlymm')

load W9Qmm-hr.mat
dtQ=datenum(DateTime);
ix5=find(dtQ==datenum(2016,7,21,00,00,00));
ix6=find(dtQ==datenum(2016,7,26,00,00,00));

figure(7)
subplot(2,1,1)
plot(FinalTime(ix1:ix2),finalhourly(ix1:ix2),FinalTime(ix1:ix2),r1acorr(ix1:ix2),FinalTime(ix1:ix2),R29corr(ix1:ix2),'o',dailydt(ix3:ix4),dailymmANN(ix3:ix4))
legend('final','r1a','R29','dailyANN')
datetick('x','mm-dd-yyyy','keeplimits')

subplot(2,1,2)
plot(dtQ(ix5:ix6),Qmmhr(ix5:ix6))
ylabel('Q obs mm/hr')
