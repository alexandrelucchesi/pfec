# #!/bin/bash
#
# Listens for HTTP connections on port 80 and HTTPS connections on port 443.
facade-server --port=8001 --ssl-port=8000 --ssl-cert=resources/ssl/cert.pem --ssl-key=resources/ssl/privkey.pem

