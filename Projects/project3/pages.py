'''
	Author: Rushabh Jhaveri
	NetID : rrj28
	Principles of Programming Languages, Spring 2018.
	Professor Louis Steinberg
'''

import sys       
import subprocess
import re

# Calls the R system specifying that commands come from file commands.R
# The commands.R provided with this assignment will read the file named
# data and will output a histogram of that data to the file pageshist.pdf
def runR( ):
    res = subprocess.call(['R', '-f', 'commands.R'])

# log2hist analyzes a log file to calculate the total number of pages
# printed by each user during the period represented by this log file,
# and uses R to produce a pdf file pageshist.pdf showing a histogram
# of these totals.  logfilename is a string which is the name of the
# log file to analyze.
#
def log2hist(logfilename):
    # fill in your code here
    
    '''
    open("filename", "mode") opens a file with filename, in specified mode:
    	- r : read-only mode.
	- w : write-only mode.
		- writes new info to the file.
		- any existing files with same name will be erased when
		  this mode is activated.
	- a : append mode.
		- used to add new data to end of file.
	- r+: special read-write mode.
		- used to handle both read and write actions
		  when working with a file.

    mode tells the interpretor and developer which way the file will be used.
    
    Returns a file object.
    '''

    # Open logfile in read-only mode.
    # Check if file exists.
    
    try:
            openedFile = open(logfilename, "r")
	    #print(openedFile)
    except IOError: # Handle errors / exceptions in opening file.
	    print("ERROR: Could not open specified file.\n")
	    return

    '''
    Print job on the printer writes a line like the following in the log file:
    
    180.186.109 code: k user: louis qux: abc xuz pages: 32 def
    
    The parts we care about:
    	- user: louis
	- pages: 32
    
    Considering each line as a string, we can say each string includes:
    	- user:<spaces><name><spaces>
	- pages:<spaces><number><spaces-or-EOL>
    
    <spaces>: one or more spaces
    <spaces-or-EOL>: one or more spaces or end-of-line
    <name>: string of digits and lowercase letters
    <number>: string of digits indicating how many pages the
     	      user printed for this print job
    
    Need to formulate a regular expression that represents the above and 
    search each line for the regex.
    
    Search for pattern/regex using imported re library.
    Use re.search()

    re.search(pattern, string, flags=0):
    	- scans through string looking for first location where
	  regex pattern produces a match
	- returns a match object if successful, None if no position in the 
	  string matches the pattern.
    '''
    # Declare list of names.

    peoplePrinting = {}

    # Build the regex.
    '''
    \s 
    	- For Unicode (str) patterns: matches whitespace cbaracters.
    
    (?P<name>...) 
    	- Similar to regular parentheses, but
	- the string matched by the group is accessible via the symbolic
	  group name "name".
    
    \d
    	- For Unicode (str) patterns: 
	- matches any Unicode decimal digit.
    '''
    pattern = 'user:\s+([0-9a-z]+)\s+.*pages:\s+(\d+)(\s+|$)'
	
    '''
    re.compile(pattern, flags=0) compiles a regular expression pattern into a regex object.
    '''
    match = re.compile(pattern)

    # .read() is required because finditer requires string param.
    text = openedFile.read()

    '''
    re.finditer(pattern, string, flags=0) runs an iterator yielding match objects
    over all non-overlapping matches for the regex pattern in string.
    The string is scanned left-right, and matches are returned in the order found.
    Empty matches are also included in the result.
    '''
    for segment in re.finditer(match, text):

	    # .group(1) refers to the first parenthesized subgroup.
	    user = segment.group(1) 

	    # second parenthesized subgroup.
	    pagesPrinted = segment.group(2)

	    if user in peoplePrinting:
		    peoplePrinting[user] += int(pagesPrinted)
	    else:
		    peoplePrinting[user] = int(pagesPrinted)


    # Open the data.example file.
    data = open("data", "w+")

    # Add all values from peoplePrinting
    for pagesPrinted in peoplePrinting.values():
	    data.write('%d\n' % pagesPrinted)
    
    # Close the data file.
    data.close()

    # Close the log file.
    openedFile.close()
    runR()
    return

if __name__ == '__main__':
    log2hist(sys.argv[1])   # get the log file name from command line

# line above may be changed to log2hist("log") to make the file name
#    always be log

