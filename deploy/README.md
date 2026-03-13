# Deploying control-tower

There are several options for deploying control-tower to production. Currently, the following are documented:

* [docker-compose](./docker-compose/README.md)
* [podman quadlets](./podman/README.md)

## Understanding how the containers work together

In all cases, there are two containers, frontend and backend, with the frontend doing SSL termination, serving the 
static assets and proxying the REST/WS endpoints of the backend container. The backend is not exposed directly but 
serves the actual requests.

Data inside the containers are laid out as follows:

| container | host path          | path                                 | what                                                                                                                                       |
|-----------|:-------------------|--------------------------------------|--------------------------------------------------------------------------------------------------------------------------------------------|
| frontend  | secret or ~ui/file | configurable                         | SSL private + public keys (referenced in nginx configuration)                                                                              |
| frontend  | ~/ui/nginx.conf    | /etc/nginx/nginx.conf                | the nginx configuration that gets mounted into the container                                                                               |
| backend   | ~/db               | /app/db (configurable)               | the three sqlite database files                                                                                                            |
| backend   | N/A                | N/A                                  | environment variables `CT_*` (see [application.conf](../server/src/main/resources/application.conf) if using docker-compose with env file) |
| backend   | configurable       | application.prod.conf (configurable) | the secret configuration which includes ESI tokens and encryption keys                                                                     |
