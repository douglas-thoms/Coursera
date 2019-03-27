% Use this editor as a MiniZinc scratch book

%create array
array[int] of int: readings;
int: start;
int: n = length(readings);

set of int: HOUR = 1..n;
set of int: HOUR0 = 0..n;

array[HOUR0] of var int: temp;

constraint temp[0] = start;

constraint forall (h in HOUR)(temp[h] = (temp[h-1] + readings[h]) div 2);

%out is an arrary of temp. starting with start temp and giving temp each hour

solve satisfy;

output[show(temp)];