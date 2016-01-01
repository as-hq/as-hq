
from AS.kernel.kernel import ASKernel
from AS.kernel.shell import ASShell

if __name__ == '__main__':
  print("\nPython kernel started.\n")
  # kernel = ASKernel()

  # while True:
  #     kernel.handle_incoming()
  shell = ASShell()
  shell.run_cell('print 10; 1+1', 'sheetid')
  shell.run_cell('1+a', 'sheetid')