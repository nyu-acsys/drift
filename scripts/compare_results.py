def convert_to_dict(lines):
	d = {}
	for i in range(len(lines)):
		line = lines[i].split(",")
		d[line[1]+","+line[0]] = line[2:]

	return d

new = open("../result/non-sensitive/unv/res-polka_st-standard.csv").readlines()[1:]
old = open("0-baseline.csv").readlines()[1:]

minimum = 1e9
maximum = 0
total = 0
good = 0
bad = 0

new = convert_to_dict(new)
old = convert_to_dict(old)

print("status", "benchmark", "test_class", "new_result")

for key, newValue in new.items():
	
	assert key in old, "Baseline file doesn't have {key}"
	oldValue = old[key]

	truth = newValue[0]

	newtime, newres = 300, ""
	oldtime, oldres = 300, ""

	try:
		newres = newValue[2]
		newtime = float(newValue[1])
	except ValueError:
		pass
	try:
		oldres = oldValue[2]
		oldtime = float(oldValue[1])
	except ValueError:
		pass

	timeDiff = newtime-oldtime

	minimum = min(minimum, timeDiff)
	maximum = max(maximum, timeDiff)
	total += timeDiff
	if (newres!=oldres):
		if (newres == "T"):
			good += 1
			print("good", key, truth, newres) 
		else:
			bad += 1
			print("bad", key, truth, newres)


print(f"minimum increase in time: {minimum}, maximum increase in time: {maximum}, total increase in time: {total}, # better results: {good}, # worse results: {bad}")
