#-----------------------------------------------------------------------------
#  Server logic
#-----------------------------------------------------------------------------

# from AS.kernel.kernel import ASKernel
from IPython.core.interactiveshell import InteractiveShell

if __name__ == '__main__':
  print("\nPython kernel started.\n")
  # init AS kernel instance
  # kernel = ASKernel()
  shell = InteractiveShell(user_ns={'a':1,'b':2})
  result = shell.run_cell('casdf=a+b', silent=True)
  print result
  result2 = shell.run_cell('b+c', silent=True)
  print result2
  # print shell.object_inspect_text('a')

  # while True:
      # kernel.handleIncoming()