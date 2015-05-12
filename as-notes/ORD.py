class ORD(object):
    def __init__(self,ordName,(bid,ask)):
        self.name=ordName
        self.bid=bid
        self.ask=ask
    def __repr__(self):
        return "ORD: "+self.name+" ("+str(self.bid)+", "+str(self.ask)+")"

    
