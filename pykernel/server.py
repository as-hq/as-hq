#-----------------------------------------------------------------------------
#  Server logic
#-----------------------------------------------------------------------------

# from AS.kernel.kernel import ASKernel
from AS.kernel.shell import ASShell

if __name__ == '__main__':
  print("\nPython kernel started.\n")
  # init AS kernel instance
  # kernel = ASKernel()
  shell = ASShell(user_ns={'a':1,'b':2})

  shell.init_sheet_ns('sheetid')

  result = shell.run_header('c=a+b', 'sheetid')
  shell.init_sheet_ns('sheetid2')
  result = shell.run_cell('c+b', 'sheetid2')

  print result.__dict__
  # shell.run_cell('d=c+b\nd')
  # result2 = shell.run_cell('c', isolated=True)
  # print result
  # print result2
  # print shell.object_inspect_text('a')

  # while True:
      # kernel.handleIncoming()