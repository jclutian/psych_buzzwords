# Josua Lutian
# Jan 24 2018
# Use a csv full of schools and coordinates to fill out the new JESP dataset
# taking 2 csv files and matches the schools
import csv
import numpy as np
import re
import json
import urllib.parse
from urllib.request import urlopen

class LocationFinder:

	def __init__ (self, filename, schoolDict ) :
		self.dict = {}
		self.data = None
		self.newMat = None
		self.total = 0
		self.apiUrl =""
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
			self.dict[ re.sub('[^a-zA-Z.\d\s]', '', row[0].lower().replace(" ", "")) ] = [row[1],row[2],row[3]]

	def findLocation(self):
		# where the coordinates and addresses will be placed
		coords = np.zeros ((self.data.shape[0],2))
		add = np.zeros ((self.data.shape[0],1),'U200')
		for i in range (self.data.shape[0]):
			schoolName = re.sub('[^a-zA-Z.\d\s]', '', self.data[i,5].lower().replace(" ", ""))
			if (schoolName in self.dict):
				print ("used dict")
				coords[i,0] = self.dict[schoolName][0]
				coords[i,1] = self.dict[schoolName][1]
				add[i,0] = self.dict[schoolName][2]
			else:
				print ("used api")
				self.apiUrl = self.makeApiUrl(self.data[i,5])
				try:
					response = urllib.request.urlopen(self.apiUrl).read().decode('utf-8')
					results = json.loads(response)
				except:
					print (self.apiUrl)
					results = {}
					results["status"] = "Nope"
					pass
				if (results["status"]== "OK"):
					lat = results["results"][0]["geometry"]["location"]["lat"]
					lng = results["results"][0]["geometry"]["location"]["lng"]
					address = results["results"][0]["formatted_address"]
					coords[i,0] = lat
					coords[i,1] = lng
					add[i,0] = address
					self.dict [schoolName] = [lat, lng, address]
				else:
					coords[i,0] = 0.0
					coords[i,1] = 0.0
					add[i,0] = "Unknown"
					self.dict [schoolName] = [0.0, 0.0, "Unknown"]


		#merging all together
		self.newMat = np.array(np.hstack ( ( self.data,coords,add ) ) )
		#print (newMat)

	def makeApiUrl(self,address):
		#split the address into components
		comp = ""
		strList = str.split (address, " ")
		comp = urllib.parse.quote (address)
		key = "ADD API KEY HERE" 
		url = "https://maps.googleapis.com/maps/api/geocode/json?address=%s&key=%s"%(comp,key)
		print (url)
		return url

	def makeCsv(self, dataset):
		headers = ["auts","paperNum", "year", "row","firstName", "ins", "lat","lng", "address"]
		with open('JPSP1315+COORDS.csv', 'w', newline='') as csvfile:
			coordWriter = csv.writer(csvfile, delimiter=',', quoting=csv.QUOTE_MINIMAL)
			coordWriter.writerow(headers)
			for row in dataset:
				coordWriter.writerow(list(row))

	def main(self):
		print ("Doing it")
		#c = getCoords(d)
		self.makeCsv(self.newMat)

if __name__ == "__main__":
	find = LocationFinder ("+Authors+Institutions.csv","schooldict.csv")
	find.findLocation()
	find.main()
