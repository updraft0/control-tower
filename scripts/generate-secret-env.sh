#!/bin/sh -eu
# Generates the secret.env file contents. See application.conf for how the environment variables are used
echo "CT_AUTH_SECRET=$(head -c 32 /dev/urandom | python -m base64)}"
echo "CT_ESI_CALLBACK_SECRET=$(head -c 32 /dev/urandom | python -m base64)"
echo "CT_ENCRYPTION_SECRET=$(head -c 32 /dev/urandom | python -m base64)"
echo "CT_ESI_CLIENT_ID=<insert here>"
echo "CT_ESI_CLIENT_SECRET=<insert here>"
