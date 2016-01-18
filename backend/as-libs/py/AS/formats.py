from colour import Color

class Format:
    def __init__(self, 
                 bold=None, italic=None, underline=None, 
                 url=None, 
                 textColor=None, fillColor=None): 
        # must all be either True, False, or None
        self.bold = bold
        self.italic = italic
        self.underline = underline
        # must be a string or None
        self.url = url
        # must be of type Color or None
        self.textColor = textColor
        self.fillColor = fillColor

    def __repr__(self):
        attrNames = ['bold', 'italic', 'underline', 'url', 'textColor', 'fillColor']
        shownAttrs = [self.__showAttribute(attrName) for attrName in attrNames]
        nonEmptyShownAttrs = [x for x in shownAttrs if x != None]
        return repr(nonEmptyShownAttrs)

    def __showBold(self):
      if self.bold == True: 
        return "Bold"
      else: 
        return None

    def __showItalic(self):
      if self.italic == True: 
        return "Italic"
      else: 
        return None

    def __showUnderline(self):
      if self.underline == True: 
        return "Underline"
      else: 
        return None

    def __showUrl(self):
      if self.url != None: 
        return "URL { urlLink = " + self.__haskellShow(self.url) + " }"
      else: 
        return None

    def __showTextColor(self):
      if self.textColor != None: 
        return "TextColor " + self.__haskellShow(self.textColor.get_hex_l())
      else: 
        return None

    def __showFillColor(self):
      if self.fillColor != None: 
        return "FillColor " + self.__haskellShow(self.fillColor.get_hex_l())
      else: 
        return None

    # at least a sufficient facsimile, hopefully
    def __haskellShow(self, s):
      return '"' + s.replace('\\', '\\\\').replace('"', '\\"') + '"'

    def __capitalizeFirstLetter(self, s):
      if len(s) == 0:
        return s
      return s[0].upper() + s[1:]

    def __showAttribute(self, attrName):
      method_name = "_Format__show" + self.__capitalizeFirstLetter(attrName)
      return getattr(self, method_name)()

# #needsrefactor not the very best way, according to http://stackoverflow.com/questions/649454/what-is-the-best-way-to-average-two-colors-that-define-a-linear-gradient
# but for now probably good enough
def colorAverage(col1, col2, x): 
  return Color(rgb=(col1.red*x   + col2.red*(1-x), 
                    col1.green*x + col2.green*(1-x),
                    col1.blue*x  + col2.blue*(1-x)))