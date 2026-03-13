# Deploying with docker-compose

* If using SSL (aka not in local dev), generate the SSL certificates according to the instructions (LetsEncrypt or CDN reverse proxy)
* Run the generate secret scripts in [`scripts`](../../scripts) and fill in the ESI token and encryption secret details
* Create directories `db` and `ui` under the user on the host you are trying to run as
  * `db` needs to be writeable since that is where the sqlite database files go
  * `ui` is read-only as that is where `nginx.conf` and SSL keys live
* Modify `compose.yaml` as appropriate (e.g. to set memory settings or mount SSL private/public key files)
* Run `docker-compose up` and you should be good to go
