#!/bin/bash
#
# Listens for HTTP connections on port 80 and HTTPS connections on port 443.
#
auth-server --port=9001 --ssl-port=9000 --ssl-cert=resources/ssl/cert.pem --ssl-key=resources/ssl/privkey.pem

