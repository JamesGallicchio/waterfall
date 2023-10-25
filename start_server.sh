#!/usr/bin/bash

./ngrok http 127.0.0.1:20080 --domain=sunfish-flowing-logically.ngrok-free.app > /dev/null &

./build/bin/server 20080

wait
