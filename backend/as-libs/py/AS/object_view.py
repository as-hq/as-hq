from __future__ import print_function
import json
import traceback
import sys
import pprint

from AS.iterable import ASIterable
from AS.hidden import *
from AS.errors import *
from AS.formats import *

def object_view(o):
  # print(str(o), file=sys.__stdout__)
  if isinstance(o, Hidden):
    return object_view(o.unhide())
  if isinstance(o, dict):
  	# the default json parser seems better in this case, for nested dicts
  	return json.dumps(o, indent = 2)
  if isinstance(o, basestring):
    width = 80
    lines = [o[i:i+width] for i in range(0, len(o), width)]
    return '\n'.join(lines)
  return pprint.pformat(o)