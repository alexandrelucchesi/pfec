#!/bin/bash

currentDir=$(pwd)

cd $currentDir/auth-server; sh run-server.sh &
cd $currentDir/facade-server; sh run-server.sh &
cd $currentDir/rest-client; sh run-client.sh

