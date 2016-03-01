#!/bin/bash

# This script is run hourly on the server, first argument is interface (eth0)

# Calculate CPU, RAM, and DISK_USE stats using standard utilities like df, free, and top
CPU=`top -b -n1 | grep "Cpu(s)" | awk '{print $2 + $4}'`
FREE_MEM=`free -m | grep Mem`
CURRENT=`echo $FREE_MEM | cut -f3 -d' '`
TOTAL=`echo $FREE_MEM | cut -f2 -d' '`
RAM=`echo "scale = 2; $CURRENT/$TOTAL*100" | bc`
DISK_USE=`df -lh | awk '{if ($6 == "/") { print $5 }}' | head -1 | cut -d'%' -f1`

# Calculate an average network PPS
TXSUM=0
RXSUM=0
INTERFACE=$1
for i in {1..10}
do
  R1=`cat /sys/class/net/$INTERFACE/statistics/rx_packets`
  T1=`cat /sys/class/net/$INTERFACE/statistics/tx_packets`
  sleep 1
  R2=`cat /sys/class/net/$INTERFACE/statistics/rx_packets`
  T2=`cat /sys/class/net/$INTERFACE/statistics/tx_packets`
  TXPPS=`expr $T2 - $T1`
  RXPPS=`expr $R2 - $R1`
  TXSUM=$(($TXSUM + $TXPPS))
  RXSUM=$(($RXSUM + $RXPPS))
done
TXAVG=$((TXSUM/10))
RXAVG=$((RXSUM/10))

# ge only works with integers, and we don't care for too much precision here anyway
# Send Anand a message if something seems bad
if [ ${CPU%.*} -ge 80 ] || [ ${RAM%.*} -ge 80 ] || [ ${DISK_USE%.*} -ge 50 ] || [ $TXAVG -ge 70 ] || [ $RXAVG -ge 70 ] ; then
  bash send-slack.sh "The server is in bad shape; CPU: $CPU %, RAM: $RAM %, DISK_USE: $DISK_USE %, Transmitted average pps: $TXAVG, received average pps: $RXAVG" "@anand" "server-bot"
fi


