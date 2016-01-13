import json

from AS.kernel.kernel import ASKernel
from AS.kernel.shell import ASShell

if __name__ == '__main__':
  print("\nPython kernel started.\n")
  
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
    print("Using address '" + addr + "' from Environment.json\n")
    kernel = ASKernel(address=addr)
  else:
    print("No environment specified, falling back on defaults\n")
    kernel = ASKernel()

  while True:
      kernel.handle_incoming()

# debugging cruft
# pls leave

  # shell = ASShell(user_ns=kernel.get_initial_ns())
#   print shell.run_header('print "hi"; a=1; a', 'sheetid2')
  # print shell.run_raw('{"a":1}', 'sheetid')
#   print shell.run_cell('print "fuck you"', 'sheetid')
#   expr = """
# class A(object):
#   def __init__(self):
#     self.x = 5
# A()
# """
#   print shell.run_cell(expr, 'sheetid')
#   print shell.run_cell(expr, 'sheetid2')
#   print shell.run_cell('a=1\na', 'sheetid')
#   print shell.run_cell('a', 'sheetid2')

  # print shell.run_cell('plt.plot([1,2,3],[4,5,6])', 'sheetid')

