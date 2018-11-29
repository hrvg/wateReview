from validate_email import validate_email

def flatten(var,vartype='list'):
	if vartype == 'list':
		var=[item for sublist in var for item in sublist]
	elif vartype == 'array':
		var=np.asarray([item for sublist in var for item in sublist])
	return var

lang = "portuguese"
file = "./data/mails_" + lang +".csv"
f = open(file,'r') # open file
data_raw = f.readlines() # read lines, return a list, each element is a str
f.close()
data = [row.split('\r\n') for row in data_raw]
data = [row.split('"') for row in flatten(data)]
data = [row.split('(') for row in flatten(data)]
data = [row.split(')') for row in flatten(data)]
data = [row.split('\\') for row in flatten(data)]
data = [row.split(',') for row in flatten(data)]
data = [row.split(';') for row in flatten(data)]
data = [row.split('*') for row in flatten(data)]
data = flatten(data)

mails = [elmt+'\n' for elmt in data if validate_email(elmt)]
mails = [elmt for elmt in mails if len(elmt) > 10]
filename = open("./data/mails_" + lang +"_fmt.d",'w')
filename.writelines(mails)
filename.close()