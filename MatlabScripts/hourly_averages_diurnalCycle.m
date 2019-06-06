smearDataSo2=importdata("smeardata_20170601120000_SO2_16.csv")
smearDataParts=importdata("smeardata_20170601120000_allPart.csv")

workData=smearDataSo2.data
% % creating a new datetime column
% dt_column_SO2=datetime(smearDataSo2.data(:,1:6));
% dt_column_Parts=datetime(smearDataParts.data(:,1:6));

so2means=[]
partmeans=[]
so2_errs= []
part_errs=[]
for i = 0:23;
    vi=find(smearDataSo2.data(:,4)==i);
    help=smearDataSo2.data(vi,7);
    so2means=[so2means nanmean(help)];
    so2_errs=[so2_errs std(help,'omitnan')/sqrt(length(help))]
    
    vi=find(smearDataParts.data(:,4)==i);
    help=smearDataParts.data(vi,7);
    partmeans=[partmeans nanmean(help)];
    part_errs=[part_errs std(help,'omitnan')/sqrt(length(help))]
end

yyaxis left
errorbar(0:23,so2means,so2_errs)
ylabel('SO2 [ppb]')
yyaxis right
errorbar(0:23,partmeans,part_errs)
xlabel('hour of the day')
ylabel('total particles [1/cm3]')
xticks(1:2:23)
title('SO2 - total particles, June 2017')


% [workData(:,1) workData(:,2), workData(:,3), workData(:,5)] = deal(0);
% plot(workData(:,end),'x','DatetimeTickFormat','HH')
% 
% 
% 
% % Make timetable
% SO2_timetable = timetable(dt_column_SO2,smearDataSo2.data(:,end),'VariableNames',{'Data'});
% parts_timetable = timetable(dt_column_Parts,smearDataParts.data(:,end),'VariableNames',{'Data'});
% 
% 
% SO2_hourly=rmmissing(retime(SO2_timetable,'hourly',@(x)mean(x,'omitnan')));
% parts_hourly=rmmissing(retime(parts_timetable,'hourly', @(x)mean(x,'omitnan')));
% 
% TT2 = retime(SO2_timetable,'hourly','mean')
% 
% test=smearDataSo2.data(:,3)
% test2=test(test == 10)
% 
% yyaxis left
% plot(SO2_hourly.dt_column_SO2,SO2_hourly.Data)
% yyaxis right
% plot(parts_hourly.dt_column_Parts,parts_hourly.Data)
