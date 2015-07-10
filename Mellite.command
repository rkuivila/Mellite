#!/bin/sh
cd "`dirname $0`"
java -Xms256m -Xmx2048m -XX:PermSize=256m -XX:MaxPermSize=512m -server -jar Mellite.jar "$@"
