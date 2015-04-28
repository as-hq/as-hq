class ORD(object):
    def __init__(self,ordName,(bid,ask)):
        self.name=ordName
        self.bid=bid
        self.ask=ask

    #reps
    @classmethod
    def deserialize(cls, js):
        return cls(js["name"], (js["bid"], js["ask"]))

    def serialize(self):
        return str({ "name": self.name, "bid": self.bid, "ask": self.ask })

    def displayValue(self):
        return "ORD: "+self.name+" ("+str(self.bid)+", "+str(self.ask)+")"

    def __str__(self):
        return str({ "displayValue": self.displayValue(), "actualValue": { "objectType": "ORD", "jsonRepresentation": self.serialize() } })

    def __repr__(self):
        return str(self)
