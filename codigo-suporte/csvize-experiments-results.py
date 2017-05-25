#!/usr/bin/python

import csv, re
import sys, getopt

# exp = chunk_size_evaluation, execution = 1, benchmark = vectoradd, size_of_data = LARGE_DATASET, schedule = DYNAMIC, chunk_size = 32, num_threads = 12,
# version = OMP, num_threads = 12, N = 2048, ORIG = 80660019349, OMP = 9526352319

header = 'exp,execution,benchmark,size_of_data,schedule,chunk_size,num_threads,version,num_threads,N,ORIG,OMP'

def parse(data):

    print 'parsing...'

    result = re.findall('exp = (.*?), execution = (.*?), benchmark = (.*?), size_of_data = (.*?), schedule = (.*?), chunk_size = (.*?), num_threads = (.*?),\n*version = (.*?), num_threads = (.*?), N = (.*?), ORIG = (.*?), OMP = (.*?), \n', data, re.DOTALL)

    print (result)

    return result

def write_to_csv(parsed_data, header, filename):
    with open(filename, 'w') as f:
        f.write(header + '\n')
        writer = csv.writer(f, lineterminator='\n')
        for item in parsed_data:
            writer.writerow(item)

def main(argv):
   inputfile = ''
   outputfile = ''
   try:
      opts, args = getopt.getopt(argv,"hi:o:",["ifile=","ofile="])
   except getopt.GetoptError:
      print 'csvize.py -i <inputfile> -o <outputfile>'
      sys.exit(2)
   for opt, arg in opts:
      if opt == '-h':
         print 'csvize.py -i <inputfile> -o <outputfile>'
         sys.exit()
      elif opt in ("-i", "--ifile"):
         inputfile = arg
      elif opt in ("-o", "--ofile"):
         outputfile = arg
   
   if inputfile and outputfile :
      print 'Input file is "', inputfile
      print 'Output file is "', outputfile
      with open(inputfile, 'r') as f:
    	data = f.read()
    	print (data)
      result = parse(data)
      print 'writing...'
      write_to_csv(result, header, outputfile)
   else :
      print 'Use:\ncsvize.py -i <inputfile> -o <outputfile>'

if __name__ == "__main__":
   main(sys.argv[1:])