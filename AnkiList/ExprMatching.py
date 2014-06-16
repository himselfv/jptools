# Parses expression and reading records.
# Sometimes in places where we expect expression, a clarification is needed
# and reading is added in one of various ways:
#   <ruby>側<rt>そば</rt></ruby>
#   側 [そば]
#   側（そば）
#   側・そば
# Sometimes it's excessive (e.g. it was only a visual clue and we have this data
# from another column), at other times important.
# 
# We also support multi-value splitting here, e.g.
#   側 [がわ、そば]
# In case someone thinks several readings/writings are interchangeable, but only
# to a limited extent. We do not support full-blown EDICT style kana-to-kanji
# matching:
#   側、侧 [がわ(侧、側)、そば(側)] --- not supported
# 
# Usage:
#   SetReadingPattern('your inline-reading regex') # optionally
#   read = StripInlineReading(expr)
#   read = StripInlineReading(expr)

import re

# Default values
ExpressionSeparator = '、'
ReadingSeparator = None # same as expression
ExpressionPattern = ( '(?J)(?|'
    '<ruby>(?''expr''[^<>]*)<rt>(?''read''[^<>]*)</rt></ruby>'+'|'
    '(?''expr''[^\[]*)\s*\[(?''read''[^\]]*)\]'+'|'
    '(?''expr''[^（]*)\s*（(?''read''[^）]*)）'+'|'
    '(?''expr''[^・]*)\s*・s*(?''read''.*)'
    '?)' )
ReadingPattern = '' # no special parsing

# Overrides default matching patterns with given regex.
# It must match any required expression+reading pattern and put expression
# into named group 'expr' and reading into 'read'.
# Separate function to allow for caching of internal objects.
def SetExpressionPattern(pattern):
	global ExpressionPattern
	ExpressionPattern = pattern

def SetReadingPattern(pattern):
	global ReadingPattern
	ReadingPattern = pattern

# Matches ONE expression inplace, removing everything not matched.
# Returns a tuple (expr, read) where read may be empty if not provided.
def MatchExpression(expr):
	global ExpressionPattern
	if ExpressionPattern == '':
		return ('', '')
	
	match = re.match(ExpressionPattern, expr)
	if match == None:
		return ('', '')
	
	dict = match.groupdict()
	if 'expr' in dict:
		expr = dict['expr']
	else:
		return ('', '')
	if 'read' in dict:
		read = dict['read']
	else:
		read = ''
	return (expr, read)


# Returns string
def MatchReading(expr):
	global ReadingPattern
	if ReadingPattern = '':
		return ''
	
	match = re.match(ReadingPattern, expr)
	if match == None:
		return ''
	
	dict = match.groupdict()
	if 'read' in dict:
		return dict['read']
	else:
		return ''


class Expression:
	def __init__(self):
		self.expr = ''
		self.readings = []

def SplitReadings(read):
	global ExpressionSeparator, ReadingSeparator
	if ReadingSeparator == None:
		return read.split(ExpressionSeparator)
	else:
		return read.split(ReadingSeparator)

def find_in_iterable(x, iterable):
    for i, item in enumerate(iterable):
        if item == x:
            return i
    return None

# Merges to a single list all readings used in expressions in a given list
def MergeReadings(expressions):
	ret = []
	for expr in expressions:
		for read in expr.readings:
			if find_in_iterable(read, ret) == None:
				ret.append(read)


# Splits the string with the appropriate separator and matches all items
# with MatchExpression/Reading.
# Returns a list of Expression
def ParseExpressions(expr, read=None):
	global ExpressionSeparator
	if ExpressionSeparator != None:
		parts = expr.split(ExpressionSeparator)
	else:
		parts = [expr]
	
	entries = []
	for part in parts:
		pair = MatchExpression(part)
		entry = Expression()
		entry.expr = pair[0]
		if pair[1] != '':
			entry.readings = SplitReadings(pair[1])
		else:
			entry.readings = None
		entries.append(entry)
	
	if read == None:
		return entries
	
	# Else additionally parse common readings
	readings = [MatchReading(x) for x in SplitReadings(read)]

	# Copy common readings to expressions where no exact readings are given
	for entry in entries:
		if entry.readings == None:
			entry.readings = readings

	# Check that all the external readings are used or it's a warning
  	all_readings := MergeReadings(Result);
  	for r in all_readings:
  		if find_in_iterable(r, readings) == None:
  			print >> sys.stderr, 'Warning: reading '+r+' not assigned to any kanji in "'+expr+' : '+read+'"'

class Reading:
	def __init__(self):
		self.read = ''
		self.expressions = [] # expression indexes in expr

# Converts "kanji with attached kana" to "kana with attached kanji"
# Returns a list of Reading
def ExpressionsToReadings(expr):
	readings = MergeReadings(expr)
	ret = []
	for reading in readings:
		entry = Reading()
		entry.read = reading
		for idx, expr_entry in enumerate(expr):
			if find_in_iterable(reading, expr_entry.readings) != None:
				entry.expressions.append(idx)
		if len(entry.expressions)==len(expr): #all expressions matched
			entry.expressions = None #informal "all matched"
		ret.append(entry)
	return ret
