#-----------------------------------------------------------------------------
#  Server logic
#-----------------------------------------------------------------------------

if __name__ == '__main__':
  print("\nPython kernel started.\n")
  # init AS kernel instance
  # kernel = ASKernel()
  shell = ASShell()
  shell2 = ASShell()
  x = shell.run_cell("1+1", silent=True)
  print(x)

  # while True:
      # kernel.handleIncoming()