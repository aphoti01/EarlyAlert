import csv
import string
import pandas as pd


data_file=open('studentVle.csv','rU')
reader=csv.DictReader(data_file)
data1={}
csv_columns = ['ID','Date']
for row in reader:
    if row['id_student'] in data1:
        data1[row['id_student']].append(row['date'])
    else:
        data1[row['id_student']] = [row['date']]
data_file.close()

with open('students.csv', 'w') as f:
    for key in data1.keys():
        f.write("%s,%s\n"%(key,data1[key]))




input_file = open('students.csv', 'r')
output_file = open('studentClean.csv', 'w')
data = csv.reader(input_file)
writer = csv.writer(output_file)
specials1 = ']'
specials2 = '['
specials3 = '\''
for line in data:
    line = [value.replace(specials1, '') for value in line]
    line = [value.replace(specials2, '') for value in line]
    line = [value.replace(specials3, '') for value in line]
    writer.writerow(line)
input_file.close()
output_file.close() 

file = []


###########section 2


with open('studentClean.csv','r') as f:
    dup=[]
    studentTime= {}
    studentDistance= {}
    studentMetric={}
    studentMaxDistance={}
    distance=0
    temp_distance = 0 
    insert_zero = False
    max_dist =0 
    for line in f:
        l = line.split(',')
        key = l.pop(0)
        for item in l: 
            item.replace('\n', '')
            try:
                val = int(item)
            except ValueError:
                val= -1
            if val != -99:
                if val not in dup:
                    dup.append(val)
        if len(dup)>0:
            for i in range(len(dup) - 1, -1, -1):
                if dup[i]< 0:
                    insert_zero = True
                    dup.remove(dup[i]) 
            if insert_zero:
                dup.insert(0,0)
            studentTime[key] = len(dup)
        
            for i in range(0,len(dup)-1):
                temp_distance = dup[i+1]-dup[i]
                distance = distance +temp_distance
                if (max_dist < temp_distance):
                    max_dist = temp_distance
            studentDistance[key]= distance
            studentMetric[key]= len(dup)*(distance)
            studentMaxDistance[key]= max_dist
        del dup[:]
        distance=0
        max_dist =0 
        insert_zero = False
            
    
d= ["student","weightD"]

df = pd.DataFrame.from_dict(studentMetric, orient='index')
df.to_csv("metric.csv")

df = pd.DataFrame.from_dict(studentTime, orient='index')
df.to_csv("times.csv")
        
df = pd.DataFrame.from_dict(studentDistance, orient='index')
df.to_csv("distance.csv")

df = pd.DataFrame.from_dict(studentMaxDistance, orient='index')
df.to_csv("max.csv")



#############section 3



metricdf = pd.read_csv('metric.csv')
timedf = pd.read_csv('times.csv')
distancedf = pd.read_csv('distance.csv')
maxdf = pd.read_csv('max.csv')

testdf = pd.read_csv('test.csv')
traindf = pd.read_csv('train.csv')



df=testdf.merge(maxdf, left_on='id_student', right_on='id_student')

df.to_csv("maxdisttest.csv")
df=traindf.merge(maxdf, left_on='id_student', right_on='id_student')

df.to_csv("maxdisttrain.csv")
