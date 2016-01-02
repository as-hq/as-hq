
from AS.kernel.kernel import ASKernel
from AS.kernel.shell import ASShell

if __name__ == '__main__':
  print("\nPython kernel started.\n")
  kernel = ASKernel()

  # while True:
      # kernel.handle_incoming()
  shell = ASShell()
  print shell.run_cell('def a():\n\treturn 2\na()', 'sheetid').result
