#!/usr/bin/env nu
# Generates the secret.env file contents. See application.conf for how the environment variables are used
print $"CT_AUTH_SECRET=(random binary 32 | encode base64)"
print $"CT_ESI_CALLBACK_SECRET=(random binary 32 | encode base64)"
print $"CT_ENCRYPTION_SECRET=(random binary 32 | encode base64)"
print "CT_ESI_CLIENT_ID=<insert here>"
print "CT_ESI_CLIENT_SECRET=<insert here>"
