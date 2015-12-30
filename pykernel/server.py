
from AS.kernel.kernel import ASKernel
from AS.kernel.shell import ASShell

if __name__ == '__main__':
  print("\nPython kernel started.\n")
  kernel = ASKernel()

  while True:
      kernel.handleIncoming()