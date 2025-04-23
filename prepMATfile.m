%% SCRIPT TO CONSTRUCT NM INPUTDATA IN .MAT FORMAT

clearvars

%% Data input
alldata = readtable('/media/emba/emba-2/ML_BOKI/ML_data/BOKI_NM_inputdata.csv','PreserveVariableNames',true);

ID = table2cell(alldata(:,"ID"));
label = table2array(alldata(:,"label"));

% Facial expression synchrony
f1 = table2array(alldata(:,contains(alldata.Properties.VariableNames,'AU')));
n1 = alldata(:,contains(alldata.Properties.VariableNames,'AU')).Properties.VariableNames;

% head movement synchrony
f2 = table2array(alldata(:,contains(alldata.Properties.VariableNames,["headsync","Rx","Ry","Rz"])));
n2 = alldata(:,contains(alldata.Properties.VariableNames,["headsync","Rx","Ry","Rz"])).Properties.VariableNames;

% body movement synchrony
f3 = table2array(alldata(:,contains(alldata.Properties.VariableNames,'bodysync')));
n3 = alldata(:,contains(alldata.Properties.VariableNames,'bodysync')).Properties.VariableNames;

% intrapersonal synchrony
f4 = table2array(alldata(:,contains(alldata.Properties.VariableNames,'intra')));
n4 = alldata(:,contains(alldata.Properties.VariableNames,'intra')).Properties.VariableNames;

% total movement and facial expressiveness
f5 = table2array(alldata(:,contains(alldata.Properties.VariableNames,["movement","intensity"])));
n5 = alldata(:,contains(alldata.Properties.VariableNames,["movement","intensity"])).Properties.VariableNames;

% speech
f6 = table2array(alldata(:,contains(alldata.Properties.VariableNames,"speech")));
n6 = alldata(:,contains(alldata.Properties.VariableNames,"speech")).Properties.VariableNames;

% covariates
cov  = table2array(alldata(:,["acc","rt"]));
covn = alldata(:,["acc","rt"]).Properties.VariableNames;

%% save NM structure
save('/media/emba/emba-2/ML_BOKI/NeuroMiner/BOKI_NM_inputdata.mat');


