smearDataSo2=importdata("smeardata_20170601120000_SO2_16.csv")
smearDataParts=importdata("smeardata_20170601120000_allPart.csv")

%creating a new datetime column
dt_column_SO2=datetime(smearDataSo2.data(:,1:6));
dt_column_Parts=datetime(smearDataParts.data(:,1:6));

%smearDataSo2.data=[smearDataSo2.data dt_column]
%smearDataSo2(:,end+1)=1


% Make timetable
SO2_timetable = timetable(dt_column_SO2,smearDataSo2.data(:,end),'VariableNames',{'Data'});
parts_timetable = timetable(dt_column_Parts,smearDataParts.data(:,end),'VariableNames',{'Data'});


SO2_hourly=rmmissing(retime(SO2_timetable,'hourly',@(x)mean(x,'omitnan')));
parts_hourly=rmmissing(retime(parts_timetable,'hourly', @(x)mean(x,'omitnan')));

yyaxis left
plot(SO2_hourly.dt_column_SO2,SO2_hourly.Data)
yyaxis right
plot(parts_hourly.dt_column_Parts,parts_hourly.Data)
