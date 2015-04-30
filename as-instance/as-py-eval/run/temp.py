from AS.instruments.ETF import ETF

from AS.instruments.etfArb2 import etfArb

#ADD COMMANDS HERE
from AS.stdlib import *
from AS.ui.styling import *
import json

print(repr(etfArb(ETF.loadSamples(4), ETF.convRate())))