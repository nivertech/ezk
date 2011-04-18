#!/bin/sh

case $1 in
open)
    echo "Opening port $2"
    sudo iptables -A INPUT -p tcp --dport $2 -j ACCEPT
    ;;
close)
    echo "Closing port $2"
    sudo iptables -A INPUT -p tcp --dport $2 -j DROP
    ;;
openall)
    echo "Delete all Rules"
    sudo iptables -F
    ;;
*)
    echo "port.sh open  <portnumber> \nport.sh close <portnumber> \nport.sh openall"
esac

