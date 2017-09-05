

if __name__ == '__main__':
	a = 4649253524168626
	total = 0
	counter = 0
	for x in str(a)[::-1]:
		counter += 1
		v = int(x)
		if counter % 2 == 0:
			
			v = v * 2
			if v > 9:
				v -= 9
		total += v
	print total