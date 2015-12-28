#-----------------------------------------------------------------------------
#  Server logic
#-----------------------------------------------------------------------------

# from AS.kernel.kernel import ASKernel
from IPython.core.interactiveshell import InteractiveShell
from AS.kernel.shell import ASShell

if __name__ == '__main__':
  print("\nPython kernel started.\n")
  # init AS kernel instance
  # kernel = ASKernel()
  shell = ASShell(user_ns={'a':1,'b':2})
  shell.run_cell('c=a+b\nc')
  shell.run_cell('d=c+d\nd')
  # result2 = shell.run_cell('c', isolated=True)
  # print result
  # print result2
  # print shell.object_inspect_text('a')

  # while True:
      # kernel.handleIncoming()