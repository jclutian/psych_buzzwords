# Josua Lutian
# Jan 24 2019
# Compare datasets to get gender of each author

import csv
import numpy as np
import re
import json
import urllib.parse
import string
from urllib.request import urlopen


class GenderFinder:

	def __init__ (self, filename, schoolDict ) :
		self.dict = {}
		self.data = None
		self.newMat = None
		self.total = 0
		self.apiUrl =""
		self.apiKey = ""
		self.read (filename)
		self.readDict (schoolDict)

	def read (self, filename):
		dataset = []
		fp = open (filename, 'rU')
		csvReader = csv.reader (fp)
		headers = next(csvReader)
		for row in csvReader:
			dataset.append(row)
			#print (row)
		self.data = np.matrix(dataset)

	def readDict (self, filename):
		fp = open (filename, 'rU')
		csvReader = csv.reader (fp)
		headers = next(csvReader)
		for row in csvReader:
			self.total += 1
			self.dict[ re.sub('[^a-zA-Z.\d\s]', '', row[0].lower().replace(" ", "")) ] = [row[2], row[3]]

	def findLocation(self):
		# Gender
		nameList = []
		translator = str.maketrans(dict.fromkeys(string.punctuation))
		gender = np.zeros ((self.data.shape[0],1),'U20')
		for i in range (self.data.shape[0]):
			name = self.data[i,5].lower().translate(translator)
			try:
				name = name.encode('latin-1').decode('utf8')
			except:
				try:
					name = name.encode('latin-1').decode('ascii')
				except :
					try:
						name = name.encode('latin-1').decode('latin-1')
					except:
						pass
			nameList.append(name)
			if name in self.dict:
				if (int (self.dict[name][1]) >= 70):
					gender[i,0] = self.dict[name][0]
				else:
					gender[i,0] = ">70%"


		#merging all together
		#list of names into a new csv file
		nameList = np.reshape (np.array (nameList), (self.data.shape[0], 1))
		for row in nameList:
			print (row[0])
		self.newMat = np.array(np.hstack ( ( self.data,gender ) ) )
		#print (newMat)

	def makeApiUrl(self,address):
		#split the address into components
		comp = ""
		strList = str.split (address, " ")
		comp = urllib.parse.quote (address)
		key = "PUT API KEY HERE"
		url = "https://maps.googleapis.com/maps/api/geocode/json?address=%s&key=%s" %(comp,key)
		return url

	def makeCsv(self, dataset):
		headers = ["journal","authors", "paperNum", "year","row", "firstName","gender"]
		with open('All+Journals+Gender+70%check.csv', 'w', newline='') as csvfile:
			coordWriter = csv.writer(csvfile, delimiter=',', quoting=csv.QUOTE_MINIMAL)
			coordWriter.writerow(headers)
			for row in dataset:
				coordWriter.writerow(list(row))

	def main(self):
		print ("Finished")
		#c = getCoords(d)
		self.makeCsv(self.newMat)

if __name__ == "__main__":
	find = GenderFinder ("dataset.csv","genderDict.csv")
	find.findLocation()
	find.main()
