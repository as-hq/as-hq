#ADD COMMANDS HERE
from AS.stdlib import *
from AS.ui.styling import *
import json

print(repr([green("BUY") if [4.352972177860431e-3,6.681960224144631e-2,-0.41584533754951847,0.125637702389605][i]>[-0.2792335147540065,-0.3627596066004045,6.952933144697368e-2,19.620][i] and [4.352972177860431e-3,6.681960224144631e-2,-0.41584533754951847,0.125637702389605][i]>0 else red("SELL") for i in range(4)]))