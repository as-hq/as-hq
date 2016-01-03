
from AS.kernel.kernel import ASKernel
from AS.kernel.shell import ASShell

if __name__ == '__main__':
  print("\nPython kernel started.\n")
  kernel = ASKernel()

  while True:
      kernel.handle_incoming()
  # shell = ASShell(user_ns=kernel.get_initial_ns())
#   print shell.run_cell('class A(object):\n\tdef __init__(self):\n\t\tself.x = 5\ncPickle.dumps(A())', 'sheetid')
#   expr = """
# class A(object):
#   def __init__(self):
#     self.x = 5
# A()
# """
#   print shell.run_cell(expr, 'sheetid')
#   print shell.run_cell(expr, 'sheetid2')
  # print shell.run_cell('a=1\na', 'sheetid')
  # print shell.run_cell('a', 'sheetid2')
