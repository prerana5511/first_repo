function [precip_corr,FinalTime] = correctPrecipitation(date,CumPPT,bucketDump,recharge,noise,noData,outputInterval)
format long
% Correct the precipitation based on the program ACPC by Anurags
%
% INPUT:
%   date - datenum vector [N x 1]
%   CumPPT - cumulative precip values [shielded unshielded] either [Nx2] or [Nx1]
%   bucketDump - value for the bucket dump, one for each col of precip
%   recharge - value for the recharge, one for each col of precip
%   noise - value for the noise, one for each col of precip
%   noData - value for the noData, one for each col of precip
%   outputInterval - 'same' or enter number in minutes
%
%
% The function is a straight interpretation of ACPC and not optimized for
% Matlab most likely
% 20150317 Scott Havens

[N,M] = size(CumPPT);

% set the defaults
if nargin < 7
    outputInterval = 'same';
end
if nargin < 6
    noData = -6999*ones(1,M);
end
if nargin < 5
    noise = 2.5*ones(1,M);
end
if nargin < 4
    recharge = 25*ones(1,M);
end
if nargin < 3
    bucketDump = 6.25*ones(1,M);
end


if length(bucketDump) ~= M
    error('Bucket dump needs %i values',M)
end

if length(recharge) ~= M
    error('Recharge needs %i values',M)
end

if length(noise) ~= M
    error('Noise needs %i values',M)
end

if length(noData) ~= M
    error('No data needs %i values',M)
end

if length(date) ~= N
    error('date needs %i values',N)
end
po = CumPPT;
pC = NaN(size(CumPPT));
[CPPT,PPT] = deal(zeros(size(CumPPT)));

% loop over each precip column
for m = 1:M
    
    %%% Look for first positive or non noData value %%%
    % set all previous values to the first value
    
    if noData(m) ~= 0
        
        n = 1;
        while CumPPT(n,m) == noData(m) | CumPPT(n,m) < 0;
            n = n + 1;
        end
        CumPPT(1:n-1,m) = CumPPT(n,m);
        
    end
    
    
    %%% Find Previous and Next CumPPT values which are acceptable %%%
    %%% If the difference is not more then bucket dump ie the unreasonable %%%
    %%% Values are not associted with the bucket dump then distribute the %%%
    %%% NoData values in the range. if there is bucket dump then make the %%%
    %%% CumPPT equal to previous Record cumPPT. %%%
    
    % go through each record
    for i = 1:N
        
        if CumPPT(i,m) == noData(m)
            
            % look for previous precip value greater than 0
            p = i - 1;
            while CumPPT(p,m) < 0
                p = p - 1;
            end
            
            % Look for the next good precip value
            n = i + 1;
            if i == N
                n = i;
            else
                while CumPPT(n,m) == noData(m) || CumPPT(n,m) < 0
                    n = n + 1;
                end
            end
            
            if CumPPT(p,m) - CumPPT(n,m) > bucketDump(m)
                % Bucket Dump occured during the unreasonable data values
                CumPPT(p+1:n-1,m) = CumPPT(p,m);
            else
                % There is no bucket dump during the time preiod
                % Distribute the difference among the cells
                for j = p+1:n-1
                    CumPPT(j,m) = CumPPT(p,m) + (j + p) * (CumPPT(n,m) - CumPPT(p,m)) / (n - p);
                end
            end
            
        end
        
    end
    
    %%% SCANNING CYCLE 1: REMOVES MAJOR NOISES %%%
    i = 2;
    nSucc = 5;
    while i+nSucc < N
        
        if abs( CumPPT(i-1,m) - CumPPT(i,m) ) >= noise(m)
            
            % Variation is greater than noise limit.  It is major noise
            % Find at least 5 succesive values for which variation is
            % within the noise limit
            n = i;
            flag = 1;
            while flag
                d = CumPPT(i-1,m) - CumPPT(n,m);        % difference from current
                consDiff = CumPPT(n:n+nSucc-1,m) - CumPPT(n+1:n+1+nSucc-1,m); % difference for the next 5 measurements
                
                if sum(abs(consDiff) <= noise(m)) == nSucc || n+nSucc == N
                    flag = 0;
                end
                n = n + 1;
            end
            
            if d > bucketDump(m) || d < -recharge(m)
                
                % It is a bucket dump or bucket recharge event
                PPT(i:n-1,m) = 0;
                
            elseif abs(d) < noise(m) || d > 0
                
                % Noise Signal: Sudden decrease then comes back to original
                % Make all intermittent PPT = 0
                PPT(i:n-2,m) = 0;
                PPT(n-1,m) = CumPPT(n-1,m) - CumPPT(i-1,m);
                
            else
                % Rise in precip may be associated with high intensity
                % precipitation event
                PPT(i:n-1,m) = CumPPT(i:n-1,m) - CumPPT(i-1:n-2,m);
                
            end
            
            i = n;
            
        else
            % Not a high magnitude noise
            PPT(i,m) = CumPPT(i,m) - CumPPT(i-1,m);
            i = i + 1;
        end
        
    end
    
    %%% CALCULATE THE NEW CUMULATIVE PRECIP %%%
    CPPT = cumsum(PPT,1);
    
    
    %%% SCANNING CYCLE 2: REMOVES FLUCTUATION DUE TO WIND AND TEMPERATURE. %%%
    % modified on 10/5/2005
    % Soothening loop is modified form a one step procedure to a three step procedure
    
    % THE SMOOTHING IS DONE IN THREE STEPS. ONE STARTS FROM THE BEGINING TO END OF CORRECTED
    % INSTANTUOUS PPT ARRAY AFTER SCANNING CYCLE 1, SECOND STARTS FROM THE END TO BEGINING
    % OF CORRECTED INSTANTUOUS PPT ARRAY. WHEN ANY NEGETIVE INSTANTAOUS PPT IS FOUND THEN NEGETIVE
    % VALUE IS UNIFORMLY DISTRIBUTED AT TWO PREVIOUS AND TWO FOLLOWING VALUES IF SUM OF THESE 5 VALUES
    % IS GREATER THEN 0 OTHRWISE RESIDUAL NEGETIVE VELUE IS ASSIGNED TO N+2 VALUE AND ALL THE FOUR
    % VALUES ARE MADE 0.
    
    PPT_B = PPT(1:N,m);
    PPT_E = PPT(1:N,m);
    
    % Smoothening loop that begins with the starting of the file
    if PPT_B(1) < 0
        if PPT_B(1) + PPT_B(2) > 0
            PPT_B(1:2) = (PPT_B(1) + PPT_B(2)) / 2;
        else
            PPT_B(2) = PPT_B(1) + PPT_B(2);
            PPT_B(1) = 0;
        end
    end
    
    for i = 3:N-2
        if PPT_B(i) < 0
            ind = i-2:i+2;
            if sum(PPT_B(ind)) < 0
                sm = sum(PPT_B(ind));
                PPT_B(ind) = 0;
                PPT_B(i+2) = sm;
            else
                sm = sum(PPT_B(ind))/5;
                PPT_B(ind) = sm;
            end
        end
    end
    
    % Smoothing that begins form the end of file: added 10/5/05
    if PPT_E(N) < 0
        if PPT_E(N) + PPT_E(N-1) > 0
            PPT_E(N-1:N) = (PPT_E(N-1) + PPT_E(N)) / 2;
        else
            PPT_E(N-1) = PPT_E(N-1) + PPT_E(N);
            PPT_E(N) = 0;
        end
    end
    
    for i = N-2:-1:3
        if PPT_E(i) < 0
            ind = i-2:i+2;
            if sum(PPT_E(ind)) < 0
                sm = sum(PPT_E(ind));
                PPT_E(ind) = 0;
                PPT_E(i-2) = sm;
            else
                sm = sum(PPT_E(ind))/5;
                PPT_E(ind) = sm;
            end
        end
    end
    
    % Taking average of smoothened values
    PPT(:,m) = (PPT_B + PPT_E) / 2;
    PPT(1) = 0;
    
    if PPT(2,m) < 0; PPT(2,m) = 0; end
    if PPT(N,m) < 0; PPT(N,m) = 0; end
    if PPT(N-1,m) < 0; PPT(N-1,m) = 0; end
    
    CPPTCrr(:,m) = cumsum(PPT(:,m));
    CPPTCrr(1,m) = 0;
    
    % determine the desired return format
    if isnumeric(outputInterval)
        
        % get the final time vector
        FinalTime = min(date):outputInterval/60/24:max(date);
        recordLength = length(FinalTime);
        
        [Final,FinalCumPPT,FinalCPPT,FinalPPT] = deal(zeros(recordLength,1));
        % Final - cumulative corrected precip
        % FinalCumPPT - cumulative original precip with some corrections
        % FinalCPPT - cumulative before smoothing
        j = 1; i = 1;
        while j <= recordLength
            
            if (FinalTime(j) - date(i)) < 0.000001
                % the final time cooresponds to a actual measurement time
                Final(j) = CPPTCrr(i,m);
                FinalCumPPT(j) = CumPPT(i,m);
                FinalCPPT(j) = CPPT(i,m);
                if j == 1
                    FinalPPT(j) = 0;
                else
                    FinalPPT(j) = Final(j) - Final(j-1);
                end
                i = i + 1;
                j = j + 1;
                
            else
                % interpolate the measurements to the new desired time
                if date(i-1) < FinalTime(j) && FinalTime(j) < date(i)
                    
                    Final(j) = CPPTCrr(i - 1,m) + (CPPTCrr(i,m) - CPPTCrr(i - 1,m)) * (FinalTime(j) - date(i - 1)) / (date(i) - date(i - 1));
                    
                    FinalCumPPT(j) = CumPPT(i - 1,m) + (CumPPT(i,m) - CumPPT(i - 1,m)) * (FinalTime(j) - Time(i - 1)) / (Time(i) - Time(i - 1));
                    
                    FinalCPPT(j) = CPPT(i - 1,m) + (CPPT(i,m) - CPPT(i - 1,m)) * (FinalTime(j) - Time(i - 1)) / (Time(i) - Time(i - 1));
                    
                    if j == 1
                        FinalPPT(j) = 0;
                    else
                        FinalPPT(j) = Final(j) - Final(j - 1);
                    end
                    
                    j = j + 1;
                else
                    % Find next date measurement that includes the
                    % FinalTime value
                    i = i + 1;
                end
            end
        end
        precip_corr(:,m) = Final;
        
    else
        % set values for same output
        precip_corr(:,m) = CPPTCrr(:,m);
        FinalTime = date;
    end
end

% round to tenths place
precip_corr = round(precip_corr*10)/10;

% for debugging purposes
% figure(10); clf
% subplot(2,1,1)
% plot(po)
% title('Original')
% subplot(2,1,2)
% plot(CPPTCrr)
% title('Corrected')



