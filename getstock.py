import os
import time

os.chdir("/home/ubuntu/ESDL-PROJECT/")
fd = open("stocknames.txt","r")
count = 0

start = "http://real-chart.finance.yahoo.com/table.csv?s="
end = ".BO&a=09&b=6&c=1997&d=06&e=14&f=2015&g=m&ignore=.csv"

while True:
    l = fd.readline()
    if not l:
        break
    l=l.rstrip('\n')
    pass_string = "wget -O "+l+".csv "+start+l+end
    print pass_string
    os.system(pass_string)
    time.sleep(2)
    count+=1

fd.close()
print count