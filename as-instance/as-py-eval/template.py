#ADD COMMANDS HERE

import json

def red(x):
    return {"style":"red","value":x}
def green(x):
    return {"style":"green","value":x}

class Test:
    def __init__(self, aa):
        self.a = aa
    def requestA(self):
        return "'" + self.a + "'"

    @classmethod
    def deserialize(cls, js):
        return cls(js["a"])

    def serialize(self):
        return str({ "a": self.a })

    def __str__(self):
        return str({ "displayValue": self.a, "actualValue": { "objectType": "Test", "jsonRepresentation": self.serialize() } })

    def __repr__(self):
        return str(self)
