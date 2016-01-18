class Hidden(object):
  def __init__(self, val, name):
    self.val = val
    self.name = name

  def hide(self):
    return self;

  def unhide(self):
    return self.val