from __future__ import print_function

import sys
import json

from AS.kernel.kernel import ASKernel
from AS.kernel.shell import ASShell

# a convenience function to ensure we never print to a file
# other than the canonical sys.stdout (this empirically happens
# when an evaluation is being served and another one gets 
# requested, because the shell temporarily changes sys.stdout
# in order to capture prints). 
def cprint(str):
  print(str, file=sys.__stdout__)

if __name__ == '__main__':
  cprint("\nPython kernel started.\n")
  
  settings = None
  try:
    f = open('../Environment.json', 'r')
    settings = json.loads(f.read())
    f.close()
  except:
    settings = {}

  kernel = None
  if 'pyKernelAddress_python' in settings:
    addr = settings['pyKernelAddress_python']
    cprint("Using address '" + addr + "' from Environment.json\n")
    kernel = ASKernel(address=addr)
  else:
    cprint("No environment specified, falling back on defaults\n")
    kernel = ASKernel()

  kernel.listen()

# debugging cruft
# pls leave

  # shell = ASShell(user_ns=kernel.get_initial_ns())
#   print shell.run_header('print "hi"; a=1; a', 'sheetid2')
  # print shell.run_raw('["sdf"]', 'sheetid')
#   print shell.run_cell('print "fuck you"', 'sheetid')
  # expr = """
# def f(x):
  # return x ** 2
# def g(x):
#   return f(x)
# g(10)
# """
  # cprint(shell.run_cell(expr, 'sheetid'))
  # cprint(shell2.run_cell(expr).result)
#   cprint(shell.run_cell(expr, 'sheetid2'))
  # cprint(shell.run_cell('a=1\na', 'sheetid'))
  # cprint(shell.run_cell('a', 'sheetid'))

  # print shell.run_cell('plt.plot([1,2,3],[4,5,6])', 'sheetid')

